# -*- coding: utf-8 -*-
import re

class Stemmer:
    
    def __init__(self):
        pass

    def text_to_parts(self, text):
        words = re.compile('\W+',re.U).split(text)
        parts = set() 
        for word in words:
            self.__word_to_parts(word, parts)  
        return parts 
    
    def __word_to_parts(self, word, parts):
        start = word[0:2]
        for char in word[2:]:
            start = start + char
            parts.add(start)
