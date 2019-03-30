# -*- coding: utf-8 -*-
#import os
#import csv
#import glob
#import MySQLdb
import scrapy
from scrapy.http import Request

class ReviewsSpider(scrapy.Spider):
    name = 'reviews'
    allowed_domains = ['']
    start_urls = ['https://pitchfork.com/reviews/albums/?page=%s' % page for page in range(1,1001)]

    def parse(self, response):
        reviews = response.xpath('//div[@class="review"]/a/@href').extract()

        for review in reviews:
            absolute_url = response.urljoin(review)
            yield Request(absolute_url, callback = self.parse_review, dont_filter=True)

    def parse_review(self, response):

        date = response.xpath('//time/text()').extract_first()
        label = response.xpath('//li[@class="labels-list__item"]/text()').extract()
        artist = response.xpath('//*[@class="artist-links artist-list single-album-tombstone__artist-links"]/li/a/text()').extract()
        album = response.xpath('//*[@class="single-album-tombstone__review-title"]/text()').extract()
        rating = response.xpath('//*[@class="score"]/text()').extract()
        author = response.xpath('//*[@class="authors-detail__display-name"]/text()').extract()
        genre = response.xpath('//*[@class="genre-list__link"]/text()').extract()
        review = response.xpath('//*[@class="contents dropcap"]/p/text()').extract()

        yield {
        'date_published': date,
        'label': label,
        'artist': artist,
        'album': album,
        'genre': genre,
        'rating': rating,
        'author': author,
        'review': review
        }
