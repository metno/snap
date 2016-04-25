import sys
from Resources import Resources
from MainBrowserWindow import MainBrowserWindow
from PyQt5 import QtCore, QtGui, QtWidgets, QtWebKit, QtWebKitWidgets

def snappy(argv) :
    res = Resources()
    app = QtWidgets.QApplication(argv)
    main = MainBrowserWindow()
    main.setWindowTitle('SNAPpy')
    main.setWindowIcon(QtGui.QIcon(res.getIconPath()))
    main.show()
    main.set_html(res.getStartScreen())
    sys.exit(app.exec_())

if __name__ == "__main__":
    snappy(sys.argv)
