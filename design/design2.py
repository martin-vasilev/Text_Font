# -*- coding: utf-8 -*-
"""
Created on Mon Feb  4 17:02:58 2019

@author: marti
"""
import os

if not os.path.exists('design'):
    os.makedirs('design')

nsent= 24*3
ncond= 3

for ID in range(1, 52):
    
    item= 3*[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24]
    item.sort()
    frame= [1,2,3]*int((nsent/ncond))
    
    S1= [1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49]
    S2= [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50]
    S3= [3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51]
    
    if ID in S1:
    	cond= [1,2,3]*int((nsent/ncond))
    	emot= ["negative", "neutral", "positive"]*int((nsent/ncond))
    	
    if ID in S2:
    	cond= [2,3,1]*int((nsent/ncond))
    	emot= ["neutral", "positive", "negative"]*int((nsent/ncond))
        
    if ID in S3:
    	cond= [3,1,2]*int((nsent/ncond))
    	emot= ["positive", "negative", "neutral"]*int((nsent/ncond))
    		
    c= list(zip(item, cond, frame))
    
    # randomise:
    from random import shuffle
    shuffle(c)
    pract= [(25, 9, 1), (26, 9, 1), (27, 9, 1), (28, 9, 1), (29, 9, 1), (30, 9, 1)]
    shuffle(pract)
    design= pract+ c
    print(design)
    	
    thefile = open('Design/P'+ str(ID)+ '.txt', 'w')
    thefile.write("item cond frame\n") # columns
    	
    for item in design:
    	thefile.write("%s %s %s\n" % item)
    thefile.close()