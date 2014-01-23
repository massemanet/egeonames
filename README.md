egeonames
=========

erlang wrapper around geonames data set

  download the files you want (XX.txt, where XX is the country code), and
  put them in egeonames/priv/data.

cd egeonames
CC=DK
wget http://download.geonames.org/export/dump/$CC.zip
unzip $CC.zip
mv $CC.txt priv/data/
rm $CC.zip readme.txt

  (re)start egeonames
egeonames:start().

  lookup like this;
egeonames:lookup(string(NameOfPopulatedPlace)[,string(CountryCode)])
