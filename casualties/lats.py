import numpy
import utm
import pandas
from convertbng.util import convert_lonlat

def lats(easting, northing):
    (longitude, latitude) = convert_lonlat(easting, northing, 30, 'U');
    return latitudes;

def lons(easting, northing):
    (longitudes, latitudes) = convert_lonlat(easting, northing, 30, 'U');
    return longitudes;

data = pandas.read_csv('casualty_points.csv');
data['longitude'] = 
