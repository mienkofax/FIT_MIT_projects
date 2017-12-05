#include <exception>
#include <string>

class ModulatorException : public std::exception {
public:
	ModulatorException(const std::string &message);

	std::string message() const;

private:
	std::string m_message;
};
