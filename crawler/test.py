from selenium import webdriver
from time import sleep

driver = webdriver.Chrome()
driver.get("https://www.google.com")
print(driver.title)
sleep(5)
driver.quit()