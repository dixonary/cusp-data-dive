import numpy
import utm
import pandas
from convertbng.util import convert_lonlat

def lats(easting, northing):
    (longitudes, latitudes) = convert_lonlat(easting, northing);
    return latitudes;

def lons(easting, northing):
    (longitudes, latitudes) = convert_lonlat(easting, northing);
    return longitudes;

data = pandas.read_csv('casualty_points.csv');
data['longitude'] = lons(data['Easting'], data['Northing']);
data['latitude']  = lats(data['Easting'], data['Northing']);

print(data)

data.to_csv('casualty_points_lonlat.csv', columns=('longitude', 'latitude'),
        index=False)
