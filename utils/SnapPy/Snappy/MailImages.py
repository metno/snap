from email.mime.image import MIMEImage
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import getpass
import glob
import os
from subprocess import Popen, PIPE


def sendPngsFromDir(subject, body, wdir):
    '''send all *.png images from the wdir to myself'''
    # Create the container (outer) email message.
    mailuser = "{}@met.no".format(getpass.getuser())
    msg = MIMEMultipart()
    msg['From'] = mailuser
    msg['Reply-To'] = mailuser
    msg['To'] = mailuser
    msg['Subject'] = subject
    msg.preamble = subject
    msg.attach(MIMEText(body))

    for file in glob.glob(os.path.join(wdir, '*.png')):
        # Open the files in binary mode.  Let the MIMEImage class automatically
        # guess the specific image type.
        with open(file, 'rb') as fp:
            img_msg = MIMEImage(fp.read())
        img_msg.add_header('Content-Disposition', 'attachment', filename=os.path.basename(file))
        msg.attach(img_msg)

    # Send the email via sendmail
    p = Popen(["/usr/sbin/sendmail", "-t", "-oi"], stdin=PIPE)
    p.communicate(msg.as_bytes())
