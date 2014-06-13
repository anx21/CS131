#!/usr/bin

from twisted.internet import reactor, protocol
from twisted.protocols.basic import LineReceiver
from twisted.python import log
from twisted.web.client import getPage
from twisted.application import service, internet

import time
import datetime
import logging
import re
import sys
import json

GOOGLE_API_KEY = "AIzaSyBD5h7afp_aTP2WSJ3Q1sNdf0gw4CPXNGk"
GOOGLE_API_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

PORTS = {
	"Alford" : 12200,
	"Bolden" : 12201,
	"Hamilton" : 12202,
	"Parker" : 12203,
	"Powell" : 12204
}

NEIGBHORS = {
	"Alford" : ["Parker", "Powell"],
	"Bolden" : ["Parker", "Powell"],
	"Hamilton" : ["Parker"],
	"Parker" : ["Alford", "Bolden", "Hamilton"],
	"Powell" : ["Alford", "Bolden"]
}

class ProxyHerdProtocol(LineReceiver):
	def __init__(self, factory):
		self.factory = factory

	def connectionMade(self):
		#self.transport.write("Connection established")
		self.factory.num_connections = self.factory.num_connections + 1
		logging.info("Connection established. Total: {0}".format(
			self.factory.num_connections))

	def lineReceived(self, line):
		logging.info("Line received: {0}".format(line))
		parameters = line.split(" ")
		# IAMAT
		if (parameters[0] == "IAMAT"):
			self.handle_IAMAT(line)
		# WHATSAT
		elif (parameters[0] == "WHATSAT"):
			self.handle_WHATSAT(line)
		# AT
		elif (parameters[0] == "AT"):
			self.handle_AT(line)
		# ERROR
		else:
			logging.error("invalid line received")
			self.transport.write("? {0}\n".format(line))
		return

	def handle_IAMAT(self, line):
		parameters = line.split(" ")
		if len(parameters) != 4:
			logging.error("Invalid IAMAT command: {0}".format(line))
			self.transport.write("? {0}\n".format(line))
			return

		# Process update
		cmd, client_id, pos, client_time = parameters
		time_diff =  time.time() - float(parameters[3])

		# Server response
		if time_diff >= 0:
			response = "AT {0} +{1} {2}".format(self.factory.server_name,
				time_diff, line)
		else:
			response = "AT {0} {1} {2}".format(self.factory.server_name,
				time_diff, line)

		if client_id in self.factory.clients:
			logging.info("Update from existing client: {0}".format(client_id))
		else:
			logging.info("New client: {0}".format(client_id))
		self.factory.clients[client_id] = {"msg":response, "time":client_time}
		logging.info("Server response: {0}".format(response))
		self.transport.write("{0}\n".format(response))

		# Send location updates to neighbors
		logging.info("Send location update to neighbors")
		self.locationUpdate(response)

	def handle_WHATSAT(self, line):
		parameters = line.split(" ")
		if len(parameters) != 4:
			logging.error("Invalid WHATSAT command: {0}".format(line))
			self.transport.write("? {0}\n".format(line))
			return
		cmd_WHATSAT, client_id, radius, limit = parameters

		# Check cache
		stored_response = self.factory.clients[client_id]["msg"]
		logging.info("Stored response: {0}".format(stored_response))
		cmd_AT, server, time_diff, cmd_IAMAT, client_id_2, pos, client_time = stored_response.split()

		# Perform fetch
		pos = re.sub(r'[-]', ' -', pos)
		pos = re.sub(r'[+]', ' +', pos).split()
		pos_formatted = pos[0] + "," + pos[1]

		API_request = "{0}location={1}&radius={2}&sensor=false&key={3}".format(
			GOOGLE_API_URL, pos_formatted, radius, GOOGLE_API_KEY)
		logging.info("API request: {0}".format(API_request))
		API_response = getPage(API_request)
		API_response.addCallback(callback = lambda x:(self.printData(x, client_id, limit)))

	def handle_AT(self, line):
		parameters = line.split()
		if len(parameters) != 7:
			logging.error("Invalid AT command: {0}".format(line))
			self.transport.write("? {0}\n".format(line))
			return

		cmd_AT, server, time_diff, cmd_IAMAT, client_id, pos, client_time = parameters
		# Check time stamp to stop flooding
		if (client_id in self.factory.clients) and (client_time <= self.factory.clients[client_id]["time"]):
			logging.info("Duplicate location update from {0}".format(server))
			return

		if client_id in self.factory.clients:
			logging.info("(AT) Location update from existing client: {0}".format(client_id))
		else:
			logging.info("(AT) Location update from new client: {0}".format(client_id))

		self.factory.clients[client_id] = { "msg":("{0} {1} {2} {3} {4} {5} {6}".format(
			cmd_AT, server, time_diff, cmd_IAMAT, client_id, pos, client_time)), 
			"time":client_time }
		logging.info("Added {0} : {1}".format(client_id, self.factory.clients[client_id]["msg"]))
		# Flood to neighbors
		self.locationUpdate(self.factory.clients[client_id]["msg"])
		return

	def printData(self, response, client_id, limit):
		json_data = json.loads(response)
		results = json_data["results"]
		# Limit results
		json_data["results"] = results[0:int(limit)]
		logging.info("API Response: {0}".format(json.dumps(json_data, indent=4)))
		msg = self.factory.clients[client_id]["msg"]
		full_response = "{0}\n{1}\n\n".format(msg, json.dumps(json_data, indent=4))
		self.transport.write(full_response)

	def locationUpdate(self, message):
		for neighbor in NEIGBHORS[self.factory.server_name]:
			reactor.connectTCP('localhost', PORTS[neighbor], ProxyHerdClient(message))
			logging.info("Location update sent from {0} to {1}".format(self.factory.server_name, neighbor))
		return

	def connectionLost(self, reason):
		self.factory.num_connections = self.factory.num_connections - 1
		logging.info("Connection lost. Total: {0}".format(
			self.factory.num_connections))

class ProxyHerdServer(protocol.ServerFactory):
	def __init__(self, server_name):
		self.server_name = server_name
		self.port_number = PORTS[self.server_name]
		self.clients = {}
		self.num_connections = 0
		filename = self.server_name + "_" + re.sub(r'[:T]', '_', datetime.datetime.utcnow().isoformat().split('.')[0]) + ".log"
		logging.basicConfig(filename = filename, level=logging.DEBUG)
		logging.info('{0}:{1} server started'.format(self.server_name, self.port_number))

	def buildProtocol(self, addr):
		return ProxyHerdProtocol(self)

	def stopFactory(self):
		logging.info("{0} server shutdown".format(self.server_name))

""" --- Proxy Herd Client --- """

class ProxyHerdClientProtocol(LineReceiver):
	def __init__ (self, factory):
		self.factory = factory

	def connectionMade(self):
		self.sendLine(self.factory.message)
		self.transport.loseConnection()

class ProxyHerdClient(protocol.ClientFactory):
	def __init__(self, message):
		self.message = message

	def buildProtocol(self, addr):
		return ProxyHerdClientProtocol(self)

""" ------ Main ------ """
def main():
	if len(sys.argv) != 2:
		print "Error: incorrect number of arguments"
		exit()
	factory = ProxyHerdServer(sys.argv[1])

	reactor.listenTCP(PORTS[sys.argv[1]], factory)
	reactor.run()

if __name__ == '__main__':
    main()