# -*- coding: utf-8 -*-

# replace this code when I learn a better way
class CharReplacer:
	def __init__(self):
		self.encode_chars = { u'Ç' : u'%C7', u'Ü' : u'%DC', u'Ş' : u'%DE', u'İ' : u'%DD', u'Ö': u'%D6'}
		self.x_chars = { u'ç' : u'cx', u'ü' : u'ux', u'ş' : u'sx', u'ı' : u'ix',u'i' : u'ix', u'ö': u'ox', u'-' : u'x'}

	def encode(self, word):
		for turkish_char, replacement in self.encode_chars.items():
			word = word.replace(turkish_char, replacement)
		return word

	def replace_x(self, word):
		word = word.lower()
		for turkish_char, replacement in self.x_chars.items():
			word = word.replace(turkish_char, replacement)
		return word