LAN-Chat
========

LAN Chat with erlang language.

Implementing a simple chat system console interface. Based on a central server which records (login) users associating a nick to a pid. Each user is represented by a server process user that they will send shell commands. Using functions that encapsulate the message format to start the server and user to log into the central server, send a message and log out. The central server process each user process run on a different node, so that each client has its own console. You can use a default name for the central node and have all nodes within the same host.

