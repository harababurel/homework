#ifndef PLAYLISTQT_H
#define PLAYLISTQT_H

#include <QtWidgets/QMainWindow>
#include "Controller.h"
#include <QListWidget>
#include <QFormLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QLabel>

class PlaylistQt : public QWidget //public QMainWindow
{
	Q_OBJECT

public:
	PlaylistQt(Controller& c, QWidget *parent = 0);
	~PlaylistQt();

private:
	Controller& ctrl;
	std::vector<Song> currentSongsInRepoList;

	QListWidget* repoList;
	QLineEdit* titleEdit;
	QLineEdit* artistEdit;
	QLineEdit* durationEdit;
	QLineEdit* linkEdit;
	QPushButton* addButton;
	QPushButton* deleteButton;
	QPushButton* filterButton;
	QPushButton* moveOneSongButton;
	QPushButton* moveAllSongsButton;

	QListWidget* playList;
	
	void initGUI();
	void populateRepoList();
	void populatePlaylist();
	int getRepoListSelectedIndex();

	void connectSignalsAndSlots();

private slots:
	// When an item in the list is clicked, the text boxes get filled with the item's information
	void listItemChanged();

	void addNewSong();
	void deleteSong();

	// filters the elements in the list, by the artist written in the artist edit box
	void filterRepoSongs();

	void moveSongToPlaylist();
	void moveAllSongs();
};

#endif // PLAYLISTQT_H
