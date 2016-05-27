#include "playlistqt.h"
#include <QtWidgets/QApplication>
#include "CSVPlaylist.h"

int main(int argc, char *argv[])
{
	QApplication a(argc, argv);
	Repository repo{"Songs.txt"};
	CSVPlaylist playlist;
	Controller ctrl{ repo, playlist, SongValidator{} };
	PlaylistQt w{ ctrl };
	w.show();
	return a.exec();
}
