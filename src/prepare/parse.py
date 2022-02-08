import json

f = open('../../data/wos_citations/wos-raw.json','r',encoding='utf-8').readlines()

g=open('../../gen/prepare/temp/wos_citations.csv','w')
g.write('wos_id\twos_firstrecord\twos_totalrecords\tciting_id\tyear\tjournal\tpubtype\tdoctype\n')
for c in f:
  item=json.loads(c) 
  
  wos_id=item.get('wos_id')
  wos_firstrecord=item.get('wos_first_record')
  
  try:
    if 'Bad Request' in item.get('response').get('code'): continue
  except: 
    1+1
  
  
  try:
    citations = item.get('response').get('Data').get('Records').get('records').get('REC')
    total = item.get('response').get('QueryResult').get('RecordsFound')
  except:
    1+1
    
  for cit in citations:
    
    uid=cit.get('UID')
    pubyear=cit.get('static_data').get('summary').get('pub_info').get('pubyear')
    pubtype=cit.get('static_data').get('summary').get('pub_info').get('pubtype')
    doctype=cit.get('static_data').get('summary').get('doctypes').get('doctype')
    if 'list' in str(type(doctype)): doctype = ','.join(doctype)
    
    journals = cit.get('static_data').get('summary').get('titles').get('title')
    jn=''
    for j in journals:
      if (j.get('type')=='source'): jn = j.get('content')
      
    g.write(wos_id+'\t'+str(wos_firstrecord)+'\t'+str(total)+'\t'+str(uid)+'\t'+str(pubyear)+'\t'+jn+'\t'+pubtype+'\t'+doctype+'\n')
    
g.close()

