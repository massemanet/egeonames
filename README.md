egeonames
=========

erlang wrapper around geonames data set.
data is downloaded on demand (from http://download.geonames.org/export/dump)
and cached.

  start

    application:ensure_all_started(egeonames).

  add a country

    egeonames:add_country(se).

  check which countries are loaded

    1> egeonames:which_countries().
    [dk,se]

  lookup a place

    egeonames:lookup(atom(CountryCode), string(NameOfPopulatedPlace))

  Examples;

    1> egeonames:lookup(se, "Vallberga").
    [[{2665537,<<"Vallberga">>,<<"SE">>,56.46667,13.01667}]]

    2> egeonames:lookup(se, "Valleberga").
    [[{2665474,<<"Valleberga">>,55.98333,12.95}],
     [{2665475,<<"Valleberga">>,55.43333,14.05}]]
