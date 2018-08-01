# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 21:57:44 2017

@author: fishleongxhh
"""

import numpy as np
import pandas as pd
import requests
import csv
import random
import time
import socket
import http.client
from bs4 import BeautifulSoup
import lxml
from lxml import etree
from lxml import html 
import calendar



#此函数用来爬取指定网页url的内容
def get_content(url , data = None):
    header={
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
        'Accept-Encoding': 'gzip, deflate, sdch',
        'Accept-Language': 'zh-CN,zh;q=0.8',
        'Connection': 'keep-alive',
        'User-Agent': random.choice([  
                                       'Mozilla/5.0 (Windows; U; Windows NT 5.1; it; rv:1.8.1.11) Gecko/20071127 Firefox/2.0.0.11',  
                                       'Opera/9.25 (Windows NT 5.1; U; en)',  
                                       'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)',  
                                       'Mozilla/5.0 (compatible; Konqueror/3.5; Linux) KHTML/3.5.5 (like Gecko) (Kubuntu)',  
                                       'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.8.0.12) Gecko/20070731 Ubuntu/dapper-security Firefox/1.5.0.12',  
                                       'Lynx/2.8.5rel.1 libwww-FM/2.14 SSL-MM/1.4.1 GNUTLS/1.2.9',  
                                       "Mozilla/5.0 (X11; Linux i686) AppleWebKit/535.7 (KHTML, like Gecko) Ubuntu/11.04 Chromium/16.0.912.77 Chrome/16.0.912.77 Safari/535.7",  
                                       "Mozilla/5.0 (X11; Ubuntu; Linux i686; rv:10.0) Gecko/20100101 Firefox/10.0 ",  
                      
                                     ] )
    }
    timeout = random.choice(range(10,20))
    while True:
        try:
            rep = requests.get(url,headers = header,timeout = timeout)
            rep.encoding = 'utf-8'
            time.sleep(random.choice(range(5,20)))
            # req = urllib.request.Request(url, data, header)
            # response = urllib.request.urlopen(req, timeout=timeout)
            # html1 = response.read().decode('UTF-8', errors='ignore')
            # response.close()
            break
        # except urllib.request.HTTPError as e:
        #         print( '1:', e)
        #         time.sleep(random.choice(range(5, 10)))
        #
        # except urllib.request.URLError as e:
        #     print( '2:', e)
        #     time.sleep(random.choice(range(5, 10)))
        except socket.timeout as e:
            print( '3:', e)
            time.sleep(random.choice(range(8,15)))

        except socket.error as e:
            print( '4:', e)
            time.sleep(random.choice(range(20,30)))

        except http.client.BadStatusLine as e:
            print( '5:', e)
            time.sleep(random.choice(range(20,30)))

        except http.client.IncompleteRead as e:
            print( '6:', e)
            time.sleep(random.choice(range(5,15)))

    return rep.text
    # return html_text


#此函数用于抽取网页中KTV营业表格
def get_table(bs):
    try:
        e = bs.find("div",{"data-eval-config":'{"title":"团购详情","hippoSeq":3}'}).find("script",{"type":"text/javascript"}).get_text()
        e = e.strip('window\.ktvTable=').strip(";").replace('null','None')
    except:
        e=9999#如果无法解析出表格，则返回错误标识9999
    return e

#此函数用于将营业时间赋值给数据框df的对应位置
def assign_value(df,url_tail,my_dict,from_index):
    for i in range(len(my_dict)):
        df.ix[from_index+i,'url_tail']=url_tail
        df.ix[from_index+i,'timeFrom']=my_dict[i]['timeFrom']
        df.ix[from_index+i,'timeTo']=my_dict[i]['timeTo']


#全局数据框ktv_info，存放想要爬取的所有KTV的基本信息
global ktv_info
ktv_info=pd.read_csv('ktvinfo.csv',sep=',',header=0,encoding='GB18030')
del ktv_info['Unnamed: 0']

nrow,ncol=ktv_info.shape

#全局数据框df，用于存储KTV的营业时间
global df
df=pd.DataFrame(np.zeros(nrow*10*3).reshape((nrow*10,3)),columns=['url_tail','timeFrom','timeTo'])


#下面的循环用于爬取所有KTV的营业时间
from_index=0
for i in range(609,nrow):
    url_tail=ktv_info.ix[i,'link']
    url="https://t.dianping.com"+url_tail#目标链接
    con=get_content(url)#获取网页内容
    bs = BeautifulSoup(con, 'lxml')#将字符串解析成xml
    while bs.html.head.string=='403 Forbidden':
        con=get_content(url)#获取网页内容
        bs = BeautifulSoup(con, 'lxml')#将字符串解析成xml
    e=get_table(bs)#获取KTV的团购详情表格
    #e=9999说明获取网页表格的时候有问题，该表格可能并没有写营业时间
    if e!=9999:
        exec("mydict=" + e)#抽取表格中的信息
        assign_value(df,url_tail,mydict["ktvTimes"],from_index)#将营业时间置于数据框中存储起来
        from_index=from_index+len(mydict["ktvTimes"])
    else:
        df.ix[from_index,'url_tail']=url_tail
        df.ix[from_index,'timeFrom']='No timeFrom'
        df.ix[from_index,'timeTo']='No timeTo'
        from_index=from_index+1

#去除多余的行
index0=df.index[(df.ix[:,'url_tail']!=0)]
df=df.iloc[index0,:]
####将数据写入文件
df.to_csv('dzdp_ktv.csv',sep=',',header=True,index=False,encoding='GB18030')