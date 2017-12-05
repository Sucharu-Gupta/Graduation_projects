import mysql.connector as mc
from mysql.connector import errorcode
import csv
import magic
import codecs
import xlsxwriter

## VARIABLES
username='root'
password=''
hostname= '127.0.0.1'
database_name= ''
TABLE_DEAL='deals'
TABLE_INVESTOR='investor'
TABLE_RELATION='relation'
OUTPUTFILE='casestudyresult.xlsx'
FILE_DEAL='deals.dat'
FILE_INVESTOR='investor_general.dat'
FILE_RELATION='deal_investor_relation.dat'


def createConnection():
	try:
  		connection=mc.connect(host=hostname,user=username,passwd=password,db= database_name)
		print('CONNECTION ESTABLISHED WITH DATABASE {0}'.format(database_name)) 
		return connection
	except mc.Error as err:
  		if err.errno == errorcode.ER_ACCESS_DENIED_ERROR:
    			print("Something is wrong with your user name or password")
  		elif err.errno == errorcode.ER_BAD_DB_ERROR:
    			print("Database does not exist")
			option=str(raw_input('Do You want me to create Database with name '+ database_name+ '. Type[Yes|NO] : '))
			if option.lower() == 'yes' or option.lower()=='y' or option.lower()=='ye' :
				conn= mc.connect(host=hostname,user=username,passwd=password)
				cursor=conn.cursor()
				try:
        				cursor.execute('CREATE DATABASE IF NOT EXISTS '+ database_name)
				except mysql.connector.Error as err:
        				print("Failed creating database: {}".format(err))
        				exit(1) 
				conn.commit()
				connection=mc.connect(host=hostname,user=username,passwd=password,db= database_name)
				return connection
			else:
				print("OPTION SELECTED FOR NOT CREATING DATABASE.EXITING PROGRAM")
				exit(2)
		else:
    			print(err)




def loadTable(tableName,filePath,openconnection):
	conn=openconnection.cursor()
	print('Check if Table {0} already exist'.format(tableName))
	#check if Table Exists
	#START----------------------------------------------------------------------------------------------------------------------------------------
	conn.execute('SHOW TABLES from {0} LIKE \'{1}\''.format(database_name,tableName))
	row=conn.fetchall()
	if(len(row)!=0):
		option=str(raw_input('Table with the name {0} already exist. \nDo you want to delete the existing one? Type[YES|NO] : '.format(tableName)))
		if option.lower() == 'yes' or option.lower()=='y' or option.lower()=='ye' :
			conn.execute('DROP TABLE {0}'.format(tableName))
		else:
			conn.close()
			return
	#END------------------------------------------------------------------------------------------------------------------------------------------
	

	#reading file encoding and creating Tables
	#START----------------------------------------------------------------------------------------------------------------------------------------
	print('Checking Encoding of the file')
	filetype=fileEncodingType(filePath)
	print('Encoding type is:{0}'.format(filetype))
	if(filetype=='us-ascii'):
		loadTableASCII(tableName,filePath,openconnection)
	elif(filetype=='utf-16le'):
		loadTableUTF16LE(tableName,filePath,openconnection)
	else:
		print('Function to deal with Encoding type {0} is missing'.format(filetype))
	
	#END------------------------------------------------------------------------------------------------------------------------------------------



def fileEncodingType(infile):
	file1=open(infile).read()
	m=magic.open(magic.MAGIC_MIME_ENCODING)
	m.load()
	encoding= m.buffer(file1)
	file1=""
	return encoding
	


def loadTableASCII(tableName,filePath,openconnection):
	conn=openconnection.cursor()
	count=0
	d_reader = csv.DictReader(open(filePath,'r'))
	col=d_reader.fieldnames
	columns=' VARCHAR(32),'.join(col[0].split("|"))
	col=",".join(col[0].split("|"))
	columns=columns+' VARCHAR(32)'
	conn.execute('CREATE TABLE {0}({1})'.format(tableName,columns))
	#read cs
	data=open(filePath,'r')
	for line in data:
		if count ==0:
			count=1
			continue
		row=line.split("|")
		row="','".join(row)
		row="'"+row+"'"
		conn.execute('INSERT INTO {0}({1}) values({2})'.format(tableName,col,row))
		#print('INSERT INTO {0}({1}) values({2})'.format(tableName,col,row))
	openconnection.commit()
	print('Load Successful of table {0} from file {1}'.format(tableName,filePath))


def loadTableUTF16LE(tableName,filePath,openconnection):
	conn=openconnection.cursor()
	data=codecs.open(filePath,'r','utf-16le')
	column=data.readline()
	column=column[1:].encode('ascii')
	column = column.rstrip()
	col=",".join(column.split("|"))
	column=' VARCHAR(32),'.join(column.split("|"))
	column=column+' VARCHAR(32)'
	conn.execute('CREATE TABLE {0}({1})'.format(tableName,column))
	for line in data:
		line=line.encode('ascii')
		line=line.rstrip()
		row=line.split("|")
                row="','".join(row)
                row="'"+row+"'"
                #print('INSERT INTO {0}({1}) values({2})'.format(tableName,col,row))
                conn.execute('INSERT INTO {0}({1}) values({2})'.format(tableName,col,row))
	openconnection.commit()	
	print('Load Successful of table {0} from file {1}'.format(tableName,filePath))



def Questions(table1,table2,table3,outputfile,openconnection):
	workbook=xlsxwriter.Workbook(outputfile)
	ws=workbook.add_worksheet()
	conn=openconnection.cursor()
	print("EXECUTING QUERY 1")
	query="select d.company_id, d.deal_date, group_concat(distinct rl.investor_id separator ';') investors_ids, group_concat(distinct inv.investor_name separator ';') investor_names from "+ str(table1) +" d inner join "+table3+" rl on d.deal_id = rl.deal_id inner join "+str(table2)+" inv on inv.investor_id = rl.investor_id group by d.deal_id, 1, 2"
	print (query)	
	conn.execute(query)
	print('WRITING RESULTS IN WORKSHEET 1 in file {0}'.format(outputfile)) 	
	row=conn.fetchone()
	count=0
	ws.write(0,0,'COMPANY ID')
	ws.write(0,1,'SEED DATE')
	ws.write(0,2,'SEED INVESTOR IDS')
	ws.write(0,3,'SEED INVESTORS')

	while row is not None:
		count=count+1
		row1=u','.join(row)
		row2=row1.encode('ascii')
		row3=row2.split(",")
		for i in range(len(row3)):
			ws.write(count,i,row[i])
		row=conn.fetchone()	
	ws=workbook.add_worksheet()
	print('QUERY 1 WORK IS DONE')
	print('----------------------------------------------------------------------------------------------------------------------------------------')
	print('EXECUTING QUERY 2')	
	conn=openconnection.cursor()	
	query="select inv1.investor_name , inv2.investor_name , count(t1.deal_id) cnt from "+table3+" t1 inner join "+table2+" inv1 on inv1.investor_id = t1.investor_id, "+table3+" t2 inner join "+table2+" inv2 on inv2.investor_id = t2.investor_id where t1.deal_id = t2.deal_id and t1.investor_id != t2.investor_id group by 1, 2";
	print (query)
	conn.execute(query)
	print('WRITING RESULTS IN WORKSHEET 2 in file {0}'.format(outputfile)) 	
	row=conn.fetchone()
	maxValue=0
	Closedset=set()
        while row is not None:
		i=int(row[0].encode('ascii').split(" ")[1])
		j=int(row[1].encode('ascii').split(" ")[1])
		value=int(row[2])
		ws.write(i,j,value)
		ws.write(j,i,value)
                row=conn.fetchone()
		maxValue=max(maxValue,i,j)
		Closedset.add((i,j))
		Closedset.add((j,i))
	for i in range(1,maxValue+1):
		ws.write(i,0,"INVESTOR "+str(i))
		Closedset.add((i,0))
		ws.write(0,i,"INVESTOR "+str(i))
		Closedset.add((0,i))
		ws.write(i,i,"-")
		Closedset.add((i,i))
        

	for i in range(1,maxValue+1):
		for j in range(1,maxValue+1):
			if ((i,j) not in Closedset):
				ws.write(i,j,'0')


		
			
if __name__ == '__main__':
	print("CREATING CONNECTION")	
	conn=createConnection() 
	print('----------------------------------------------------------------------------------------------------------------------------------------')
	print("Loading FILE deals.dat")	
	loadTable(TABLE_DEAL,FILE_DEAL,conn)
	print('----------------------------------------------------------------------------------------------------------------------------------------')
	print("LOADING FILE investor_general.dat")	
	loadTable(TABLE_INVESTOR,FILE_INVESTOR,conn)
	print('----------------------------------------------------------------------------------------------------------------------------------------')
	print("LOADING FILE deal_investor_relation.dat")	
	loadTable(TABLE_RELATION,FILE_RELATION,conn)
	print('----------------------------------------------------------------------------------------------------------------------------------------')
	Questions(TABLE_DEAL,TABLE_INVESTOR,TABLE_RELATION,OUTPUTFILE,conn)	
