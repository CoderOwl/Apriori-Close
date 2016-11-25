import string
import random
from random import randint

fo = open("dataset.txt", "w+")
for j in range (1, 1500):
	d = set()
	l = randint(2, 10)
	s=""
	for i in range (1, l):
		d.add(random.choice(string.ascii_uppercase))
	while d:
		s+=d.pop()
		s+=" "
	fo.write(s+"\n")
fo.close()
