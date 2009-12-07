Until I have a chance to better document everything:

./rebar compile -> builds the library
./rebar clean   -> remove all generated files
./rebar eunit   -> run the eunit tests
./rebar install -> install the library into your OTP root

Add verbose=1 to any of the above commands to see more output from rebar.

Add force=1 to forcibly install the library, even if it's previously been installed.
