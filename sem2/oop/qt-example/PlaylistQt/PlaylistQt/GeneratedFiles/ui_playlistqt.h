/********************************************************************************
** Form generated from reading UI file 'playlistqt.ui'
**
** Created by: Qt User Interface Compiler version 5.6.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PLAYLISTQT_H
#define UI_PLAYLISTQT_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QButtonGroup>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_PlaylistQtClass
{
public:
    QWidget *centralWidget;
    QMenuBar *menuBar;
    QToolBar *mainToolBar;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *PlaylistQtClass)
    {
        if (PlaylistQtClass->objectName().isEmpty())
            PlaylistQtClass->setObjectName(QStringLiteral("PlaylistQtClass"));
        PlaylistQtClass->resize(600, 400);
        centralWidget = new QWidget(PlaylistQtClass);
        centralWidget->setObjectName(QStringLiteral("centralWidget"));
        PlaylistQtClass->setCentralWidget(centralWidget);
        menuBar = new QMenuBar(PlaylistQtClass);
        menuBar->setObjectName(QStringLiteral("menuBar"));
        menuBar->setGeometry(QRect(0, 0, 600, 21));
        PlaylistQtClass->setMenuBar(menuBar);
        mainToolBar = new QToolBar(PlaylistQtClass);
        mainToolBar->setObjectName(QStringLiteral("mainToolBar"));
        PlaylistQtClass->addToolBar(Qt::TopToolBarArea, mainToolBar);
        statusBar = new QStatusBar(PlaylistQtClass);
        statusBar->setObjectName(QStringLiteral("statusBar"));
        PlaylistQtClass->setStatusBar(statusBar);

        retranslateUi(PlaylistQtClass);

        QMetaObject::connectSlotsByName(PlaylistQtClass);
    } // setupUi

    void retranslateUi(QMainWindow *PlaylistQtClass)
    {
        PlaylistQtClass->setWindowTitle(QApplication::translate("PlaylistQtClass", "PlaylistQt", 0));
    } // retranslateUi

};

namespace Ui {
    class PlaylistQtClass: public Ui_PlaylistQtClass {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PLAYLISTQT_H
