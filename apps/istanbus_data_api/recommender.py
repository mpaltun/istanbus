# -*- coding: utf-8 -*-
import lxml.html
from client import Client

class Recommender:
	def __init__(self):
		self.client = Client("harita.iett.gov.tr:80")
		self.headers = {"Content-Type" : "application/x-www-form-urlencoded"}
		self.params = "from_durak_120425012222={}&to_durak_120425012222={}&ek=_120425012222&kod=d20&gkod=965393"
		self.url = '/aktarmaBul.php'

	def recommend(self, from_stop, to_stop):
		response = self.client.post(self.url, self.params.format(from_stop, to_stop) , self.headers)
		parsed_xml = lxml.html.parse(response)

		tables = parsed_xml.xpath('//table')
		# recommendations
		recommendations = []
		for table in tables:
			trs = table.xpath('tr[not(@*)]')
			transport = []
			for tr in trs:
				# transports
				td = tr.xpath('td/text()')[1:7]
				if (len(td) == 6):
					transport.append(td)
			recommendations.append(transport)
		return recommendations