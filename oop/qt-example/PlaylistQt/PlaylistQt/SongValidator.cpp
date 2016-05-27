#include "SongValidator.h"

using namespace std;

SongException::SongException(std::vector<std::string> _errors): errors{_errors}
{
}

std::vector<std::string> SongException::getErrors() const
{
	return this->errors;
}

std::string SongException::getErrorsAsString() const
{
	string err;
	for (auto e : this->errors)
		err += e;
	return err;
}

void SongValidator::validate(const Song & s)
{
	vector<string> errors;
	if (s.getTitle().size() < 3)
		errors.push_back("The title name cannot be less than 2 characters!\n");
	if (!isupper(s.getArtist()[0]))
		errors.push_back(string("The name of the artist must start with a capital letter!\n"));
	if (s.getDuration().getMinutes() == 0 && s.getDuration().getSeconds() == 0)
		errors.push_back(string("The duration cannot be 0!\n"));
	
	// search for "www" or "http" at the beginning of the source string
	int posWww = s.getSource().find("www");
	int posHttp = s.getSource().find("http");
	if (posWww != 0 && posHttp != 0)
		errors.push_back("The youtube source must start with one of the following strings: \"www\" or \"http\"");

	if (errors.size() > 0)
		throw SongException(errors);
}
