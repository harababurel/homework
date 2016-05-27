#include "playlistqt.h"
#include <vector>
#include "Song.h"
#include "Utils.h"
#include <QMessageBox>
#include "RepositoryExceptions.h"

PlaylistQt::PlaylistQt(Controller& c, QWidget *parent) : ctrl{ c }, QWidget { parent }
{
	this->initGUI();
	this->currentSongsInRepoList = this->ctrl.getAllSongs();
	this->populateRepoList();
}

PlaylistQt::~PlaylistQt()
{

}

void PlaylistQt::initGUI()
{
	//General layout of the window
	QHBoxLayout* layout = new QHBoxLayout{this};

	// Prepare left side components - vertical layout with: 
	// - list
	// - form layout with the song data
	// - grid layout with buttons: add, delete, update, filter
	QWidget* leftWidget = new QWidget{};
	QVBoxLayout* leftSide = new QVBoxLayout{ leftWidget };

	// list
	this->repoList = new QListWidget{};
	// set the selection model
	this->repoList->setSelectionMode(QAbstractItemView::SingleSelection);
	
	// song data
	QWidget* songDataWidget = new QWidget{};
	QFormLayout* formLayout = new QFormLayout{songDataWidget};
	this->titleEdit = new QLineEdit{};
	this->artistEdit = new QLineEdit{};
	this->durationEdit = new QLineEdit{};
	this->linkEdit = new QLineEdit{};
	formLayout->addRow("&Title:", titleEdit);
	formLayout->addRow("&Artist:", artistEdit);
	formLayout->addRow("&Duration:", durationEdit);
	formLayout->addRow("&Link:", linkEdit);

	// buttons
	QWidget* buttonsWidget = new QWidget{};
	QGridLayout* gridLayout = new QGridLayout{ buttonsWidget };
	this->addButton = new QPushButton("Add");
	this->deleteButton = new QPushButton("Delete");
	this->filterButton = new QPushButton("Filter");
	
	gridLayout->addWidget(addButton, 0, 0);
	gridLayout->addWidget(deleteButton, 0, 1);
	gridLayout->addWidget(filterButton, 0, 2);

	// add everything to the left layout
	leftSide->addWidget(new QLabel{"All songs"});
	leftSide->addWidget(repoList);
	leftSide->addWidget(songDataWidget);
	leftSide->addWidget(buttonsWidget);

	// middle component: just two button - to add the songs from the reposiotory to the playlist
	QWidget* middleWidget = new QWidget{};
	QVBoxLayout* vLayoutMiddle = new QVBoxLayout{ middleWidget };
	this->moveOneSongButton = new QPushButton{ ">> Move one song" };
	this->moveAllSongsButton = new QPushButton{ ">> Move all songs" };
	QWidget* upperPart = new QWidget{};
	QWidget* lowerPart = new QWidget{};
	QVBoxLayout* vLayoutUpperPart = new QVBoxLayout{ upperPart };
	vLayoutUpperPart->addWidget(this->moveOneSongButton);
	vLayoutUpperPart->addWidget(this->moveAllSongsButton);
	vLayoutMiddle->addWidget(upperPart);
	vLayoutMiddle->addWidget(lowerPart);

	// right component: the playlist
	QWidget* rightWidget = new QWidget{};
	QVBoxLayout* rightSide = new QVBoxLayout{ rightWidget };

	// playlist
	playList = new QListWidget{};

	// two buttons
	QWidget* playlistButtonsWidget = new QWidget{};
	QHBoxLayout* playlistButtonsLayout = new QHBoxLayout{ playlistButtonsWidget };
	playlistButtonsLayout->addWidget(new QPushButton{ "&Play" });
	playlistButtonsLayout->addWidget(new QPushButton{ "&Next" });

	// add everything to the right layout
	rightSide->addWidget(new QLabel{ "Playlist" });
	rightSide->addWidget(playList);
	rightSide->addWidget(playlistButtonsWidget);

	// add the three layouts to the main layout
	layout->addWidget(leftWidget);
	layout->addWidget(middleWidget);
	layout->addWidget(rightWidget);

	// connect the signals and slots
	this->connectSignalsAndSlots();
}

void PlaylistQt::connectSignalsAndSlots()
{
	// add a connection: function listItemChanged() will be called when an item in the list is selected
	QObject::connect(this->repoList, SIGNAL(itemSelectionChanged()), this, SLOT(listItemChanged()));

	// add button connections
	QObject::connect(this->addButton, SIGNAL(clicked()), this, SLOT(addNewSong()));
	QObject::connect(this->deleteButton, SIGNAL(clicked()), this, SLOT(deleteSong()));
	QObject::connect(this->filterButton, SIGNAL(clicked()), this, SLOT(filterRepoSongs()));

	QObject::connect(this->moveOneSongButton, SIGNAL(clicked()), this, SLOT(moveSongToPlaylist()));
	QObject::connect(this->moveAllSongsButton, SIGNAL(clicked()), this, SLOT(moveAllSongs()));
}

void PlaylistQt::populateRepoList()
{
	// clear the list, if there are elements in it
	if (this->repoList->count() > 0)
		this->repoList->clear();

	for (auto s : this->currentSongsInRepoList)
	{
		QString itemInList = QString::fromStdString(s.getArtist() + " - " + s.getTitle());
		this->repoList->addItem(itemInList);
	}
	
	// set the selection on the first item in the list
	if (this->currentSongsInRepoList.size() > 0)
		this->repoList->setCurrentRow(0);
}

void PlaylistQt::populatePlaylist()
{
	// clear the list, if there are elements in it
	if (this->playList->count() > 0)
		this->playList->clear();

	for (auto s : this->ctrl.getSongsFromPlaylist())
	{
		QString itemInList = QString::fromStdString(s.getArtist() + " - " + s.getTitle());
		this->playList->addItem(itemInList);
	}
}

int PlaylistQt::getRepoListSelectedIndex()
{
	if (this->repoList->count() == 0)
		return -1;

	// get selected index
	QModelIndexList index = this->repoList->selectionModel()->selectedIndexes();
	if (index.size() == 0)
	{
		this->artistEdit->clear();
		this->titleEdit->clear();
		this->durationEdit->clear();
		this->linkEdit->clear();
		return -1;
	}

	int idx = index.at(0).row();
	return idx;
}

void PlaylistQt::listItemChanged()
{
	int idx = this->getRepoListSelectedIndex();
	if (idx == -1)
		return;
	
	std::vector<Song> songs = this->currentSongsInRepoList;
	
	// get the song at the selected index
	if (idx >= songs.size())
		return;
	Song s = songs[idx];

	this->artistEdit->setText(QString::fromStdString(s.getArtist()));
	this->titleEdit->setText(QString::fromStdString(s.getTitle()));
	this->durationEdit->setText(QString::fromStdString(s.getDuration().toString()));
	this->linkEdit->setText(QString::fromStdString(s.getSource()));
}

void PlaylistQt::addNewSong()
{
	std::string artist = this->artistEdit->text().toStdString();
	std::string title = this->titleEdit->text().toStdString();
	std::string duration = this->durationEdit->text().toStdString();
	// get minutes and seconds
	std::vector<std::string> durationTokens = tokenize(duration, ':');
	if (durationTokens.size() != 2)
	{
		QMessageBox messageBox;
		messageBox.critical(0, "Error", "The duration must have minutes and seconds, separated by \":\"!");
		return;
	}
	std::string source = this->linkEdit->text().toStdString();

	try
	{
		this->ctrl.addSongToRepository(artist, title, stod(durationTokens[0]), stod(durationTokens[1]), source);
		// refresh the list
		this->currentSongsInRepoList = this->ctrl.getAllSongs();
		this->populateRepoList();
	}
	catch (SongException& e)
	{
		QMessageBox messageBox;
		messageBox.critical(0, "Error", QString::fromStdString(e.getErrorsAsString()));
	}
	catch (DuplicateSongException& e)
	{
		QMessageBox messageBox;
		messageBox.critical(0, "Error", e.what());
	}
}

void PlaylistQt::deleteSong()
{
	std::string artist = this->artistEdit->text().toStdString();
	std::string title = this->titleEdit->text().toStdString();

	try
	{
		this->ctrl.removeSongFromRepository(artist, title);
		// refresh the list
		this->currentSongsInRepoList = this->ctrl.getAllSongs();
		this->populateRepoList();
	}
	catch (InexistenSongException& e)
	{
		QMessageBox messageBox;
		messageBox.critical(0, "Error", e.what());
	}
}

void PlaylistQt::filterRepoSongs()
{
	std::string artist = this->artistEdit->text().toStdString();
	if (artist == "")
	{
		this->currentSongsInRepoList = this->ctrl.getAllSongs();
		this->populateRepoList();
		return;
	}
	
	this->currentSongsInRepoList = this->ctrl.filterByArtist(artist);
	this->populateRepoList();
}

void PlaylistQt::moveSongToPlaylist()
{
	int idx = this->getRepoListSelectedIndex();
	if (idx == -1 || idx >= this->currentSongsInRepoList.size())
		return;

	const Song& s = this->currentSongsInRepoList[idx];
	this->ctrl.addSongToPlaylist(s);
	this->populatePlaylist();
}

void PlaylistQt::moveAllSongs()
{
	for (auto s : this->currentSongsInRepoList)
	{
		this->ctrl.addSongToPlaylist(s);
	}
	this->populatePlaylist();
}