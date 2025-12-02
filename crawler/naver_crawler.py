import csv
import re
import time
from pathlib import Path
from typing import List, Optional, Tuple

from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.remote.webelement import WebElement

from crawler.base import BaseCrawler


class NaverReviewCrawler(BaseCrawler):
    def __init__(
        self,
        review_url: str,
        output_dir: str,
        driver_path: Optional[str] = None,
        wait_timeout: int = 10,
        scroll_rounds: int = 30,
    ):
        super().__init__(output_dir)
        self.review_url = review_url
        self.driver_path = driver_path
        self.wait_timeout = wait_timeout
        self.scroll_rounds = scroll_rounds

        self.driver: Optional[webdriver.Chrome] = None
        self.wait: Optional[WebDriverWait] = None
        self.filename: str = ""
        self.rows: List[List[str]] = []

    def start_browser(self):
        if self.driver:
            return self.driver

        service = Service(self.driver_path) if self.driver_path else Service()
        self.driver = webdriver.Chrome(service=service)
        self.wait = WebDriverWait(self.driver, self.wait_timeout)
        return self.driver

    def scrape_reviews(self):
        if not self.driver or not self.wait:
            self.start_browser()

        assert self.driver is not None
        assert self.wait is not None

        self.driver.get(self.review_url)
        time.sleep(2)

        title = self._get_movie_title()
        self.filename = self._make_filename(title)

        self._click_rating_tab()
        self._scroll_main_view()
        review_ul, scroll_box = self._get_review_list()
        self._scroll_reviews(scroll_box)
        self.rows = self._collect_reviews(review_ul)

        return self.rows

    def save_to_database(self):
        if not self.rows:
            raise ValueError("No reviews scraped yet; run scrape_reviews() first.")

        target_dir = Path(self.output_dir)
        target_dir.mkdir(parents=True, exist_ok=True)

        target_path = target_dir / (self.filename or "naver_reviews.csv")

        with target_path.open("w", encoding="utf-8-sig", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(["작성자ID", "별점", "내용", "작성일자", "공감수", "비공감수"])
            writer.writerows(self.rows)

        return target_path

    def close(self):
        if self.driver:
            self.driver.quit()
            self.driver = None
            self.wait = None

    def _get_movie_title(self) -> str:
        title_el = self.wait.until(
            EC.presence_of_element_located(
                (
                    By.CSS_SELECTOR,
                    "div.title_area.type_keep._title_area span.area_text_title strong._text",
                )
            )
        )
        return title_el.text.strip()

    def _make_filename(self, raw_title: str) -> str:
        safe_title = re.sub(r'[\\/*?:"<>|]', "", raw_title).replace(" ", "_")
        return f"{safe_title}_naver_reviews.csv"

    def _click_rating_tab(self):
        try:
            rating_tab = self.wait.until(
                EC.element_to_be_clickable(
                    (
                        By.XPATH,
                        "//ul[contains(@class, 'tab_list')]//a[.//span[normalize-space()='관람평'] or normalize-space()='관람평']",
                    )
                )
            )
            rating_tab.click()
            time.sleep(2)
        except Exception:
            pass

    def _scroll_main_view(self):
        self.driver.execute_script("window.scrollBy(0, 800);")
        time.sleep(2)

    def _get_review_list(self) -> Tuple[WebElement, WebElement]:
        review_ul = self.wait.until(
            EC.presence_of_element_located((By.CSS_SELECTOR, "ul.area_card_outer._item_wrapper"))
        )
        scroll_box = self.driver.execute_script("return arguments[0].parentNode;", review_ul)
        if not scroll_box:
            scroll_box = review_ul
        return review_ul, scroll_box

    def _scroll_reviews(self, scroll_box):
        for _ in range(self.scroll_rounds):
            self.driver.execute_script("arguments[0].scrollTop = arguments[0].scrollHeight;", scroll_box)
            time.sleep(1)

    def _collect_reviews(self, review_ul) -> List[List[str]]:
        reviews = review_ul.find_elements(By.CSS_SELECTOR, "li.area_card._item")
        rows: List[List[str]] = []

        for review in reviews:
            writer_id = review.get_attribute("data-report-writer-id") or ""
            date = review.get_attribute("data-report-time") or ""

            text = ""
            try:
                text = review.get_attribute("data-report-title") or ""
                if not text:
                    text = review.find_element(
                        By.CSS_SELECTOR, "div.area_review_content span.desc_text"
                    ).text
            except Exception:
                text = ""

            star = ""
            try:
                title_text = review.find_element(
                    By.CSS_SELECTOR, "div.area_title_box div.area_text_box"
                ).text
                nums = re.findall(r"\d+", title_text)
                star = nums[-1] if nums else ""
            except Exception:
                star = ""

            sympathy = ""
            try:
                sympathy = review.find_element(
                    By.CSS_SELECTOR, "button.area_button_upvote span.this_text_number._count_num"
                ).text
            except Exception:
                sympathy = ""

            antipathy = ""
            try:
                antipathy = review.find_element(
                    By.CSS_SELECTOR, "button.area_button_downvote span.this_text_number._count_num"
                ).text
            except Exception:
                antipathy = ""

            rows.append([writer_id, star, text, date, sympathy, antipathy])

        return rows
