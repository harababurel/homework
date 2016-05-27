#pragma once
#include "Repository.h"
#include "FilePlaylist.h"
#include "SongValidator.h"
#include <memory>
#include "Undo.h"

class Controller
{
private:
	Repository& repo;
	FilePlaylist& playList;
	SongValidator validator;
	// a vector of unique_ptr of undo actions;
	// pointers are required, as we need polymorphism;
	// each add/remove action (on the repository) will be recorded in this vector
	std::vector<std::unique_ptr<UndoAction>> undoActions;

public:
	Controller(Repository& r, FilePlaylist& p, SongValidator v) : repo{ r }, playList{ p }, validator{ v } {}

	Controller(const Controller& ctrl) = delete;		// controller cannot be copied now, because it contains unique_ptr
	void operator=(const Controller& ctrl) = delete;	// same for assignment

	Repository getRepo() const { return repo; }
	std::vector<Song> getAllSongs() const { return this->repo.getSongs(); }
	PlayList& getPlaylist() const { return playList; }
	std::vector<Song> getSongsFromPlaylist() { return this->playList.getAll(); }

	/*
		Adds a song with the given data to the song repository.
		Throws: SongException - if the song is not valid
				DuplicateSongException - if there is another song with the same artist and title
				Throws: FileException - if the repository file cannot be opened.
	*/
	void addSongToRepository(const std::string& artist, const std::string& title, double minutes, double seconds, const std::string& source);

	void removeSongFromRepository(const std::string& artist, const std::string& title);

	// undoes the actions performed on the repository
	void undo();


	/*
		Adds a given song to the current playlist.
		Input: song - Song, the song must belong to the repository.
		Output: the song is added to the playlist.
	*/
	void addSongToPlaylist(const Song& song);

	// Adds all the songs from the repository, that have the given artist, to the current playlist.
	void addAllSongsByArtistToPlaylist(const std::string& artist);

	std::vector<Song> filterByArtist(const std::string& artist);

	void startPlaylist();
	void nextSongPlaylist();

	/*
		Saves the playlist.
		Throws: FileException - if the given file cannot be opened.
	*/
	void savePlaylist(const std::string& filename);

	/*
	Opens the playlist, with an appropriate application.
	Throws: FileException - if the given file cannot be opened.
	*/
	void openPlaylist() const;
};

