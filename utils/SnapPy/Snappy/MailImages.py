# SNAP: Servere Nuclear Accident Programme
# Copyright (C) 1992-2017   Norwegian Meteorological Institute
#
# This file is part of SNAP. SNAP is free software: you can
# redistribute it and/or modify it under the terms of the
# GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
from email.mime.image import MIMEImage
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
import getpass
import glob
import os
from subprocess import Popen, PIPE


def sendPngsFromDir(subject, body, wdir, receivers=None):
    """send all *.png images from the wdir to myself"""
    # Create the container (outer) email message.
    mailuser = "{}@met.no".format(getpass.getuser())
    msg = MIMEMultipart()
    msg["From"] = mailuser
    msg["Reply-To"] = mailuser

    if receivers is not None:
        mailuser = mailuser + ", " + ", ".join(receivers)
    msg["To"] = mailuser

    msg["Subject"] = subject
    msg.preamble = subject.encode("ascii", "ignore").decode("ascii")
    msg.attach(MIMEText(body, _charset="utf-8"))

    for file in glob.glob(os.path.join(wdir, "*.png")):
        # Open the files in binary mode.  Let the MIMEImage class automatically
        # guess the specific image type.
        with open(file, "rb") as fp:
            img_msg = MIMEImage(fp.read())
        img_msg.add_header(
            "Content-Disposition", "attachment", filename=os.path.basename(file)
        )
        msg.attach(img_msg)

    # Send the email via sendmail
    p = Popen(["/usr/sbin/sendmail", "-t", "-oi"], stdin=PIPE)
    p.communicate(msg.as_bytes())
