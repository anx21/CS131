./startservers.sh

sleep 2

{
	sleep 1
	echo IAMAT latte +37.322752-122.030836 1401072205.798801
	sleep 1
	echo quit
} | telnet localhost 12200

pkill -f 'python proxyherd.py Powell'

{
	sleep 1
	echo IAMAT coffee +34.151324-118.028232 1401496386.27158
	sleep 1
	echo quit
} | telnet localhost 12200

pkill -f 'python proxyherd.py Parker'

{
	sleep 1
	echo IAMAT nowhere +35-120 1401496586.27158
	sleep 1
	echo quit
} | telnet localhost 12201

{
	sleep 1
	echo WHATSAT latte 40 5
	sleep 1
	echo quit
} | telnet localhost 12202

pkill -f 'python proxyherd.py Alford'
pkill -f 'python proxyherd.py Bolden'
pkill -f 'python proxyherd.py Hamilton'