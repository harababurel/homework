#pragma once
#include <string>
#include "Song.h"
#include <vector>

class SongException
{
private:
	std::vector<std::string> errors;

public:
	SongException(std::vector<std::string> _errors);
	std::vector<std::string> getErrors() const;
	std::string getErrorsAsString() const;
};

class SongValidator
{
public:
	SongValidator() {}
	static void validate(const Song& s);
};

