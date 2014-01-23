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

  Examples;

    1> egeonames:lookup("Vallberga","SE").
    [[{2665537,<<"Vallberga">>,<<"SE">>,56.46667,13.01667}]]

    2> egeonames:lookup("Vallberga").
    [[{2665537,<<"Vallberga">>,<<"SE">>,56.46667,13.01667}]]

    3> egeonames:lookup("Kobenhavn").
    [[{2618425,<<"Copenhagen">>,<<"DK">>,55.67594,12.56553}]]

    4> egeonames:lookup("Abild").
    [[{2625066,<<"Abild">>,<<"DK">>,54.96667,8.86667}],
     [{2727665,<<"Abild">>,<<"SE">>,56.93333,12.71667}]]
