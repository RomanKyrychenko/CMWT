from email.mime.text import MIMEText
from email.mime.application import MIMEApplication
from email.mime.multipart import MIMEMultipart
from smtplib import SMTP
import smtplib
import sys
from time import gmtime, strftime

date = strftime("%Y-%m-%d", gmtime())

recipients = ['kirichenko17roman@gmail.com'] 

msg = MIMEMultipart()
msg['Subject'] = "Stepanov" + " " + date
msg['From'] = 'Roman Kyrychenko<roman.kyrychenko@corestone.expert>'
 
msg.preamble = 'Multipart massage.\n'
 
part = MIMEText("Hi, please find the attached file")
msg.attach(part)
 
part = MIMEApplication(open("/home/stepanov_youtube/Stepanov_youtube_" + date + ".xlsx", "rb").read())
part.add_header('Content-Disposition', 'attachment', filename = "Stepanov_youtube_" + date + ".xlsx")
msg.attach(part)
 

server = smtplib.SMTP("smtp.openxchange.eu:587")
server.ehlo()
server.starttls()
server.login("roman.kyrychenko@corestone.expert", "21](,r:==P")
 
server.sendmail(msg['From'], recipients , msg.as_string())
