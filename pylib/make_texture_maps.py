#!/Users/jracusin/anaconda/bin python
#
# Copyright (C) 2011  Leo Singer
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
"""
Unpack a HEALPix array into twelve tile suitable for texture mapping
"""

# Command line interface

from optparse import Option, OptionParser
parser = OptionParser(
    description = __doc__,
    usage = "%prog [options] [INPUT]",
    option_list = [
        Option("-o", "--output", metavar="FILE.png",
            help="name of output file, gets a 2-digit tile number added "
            "before extension")
    ]
)
opts, args = parser.parse_args()

if opts.output is None:
    parser.error('missing required argument: --output');
if len(args) > 1:
    parser.error('too many command line arguments')
elif len(args) == 0:
    parser.error('not enough command line arguments')
else:
    fitsfilename, = args

import PIL.Image
import healpy as hp
import numpy as np
import os

output_name, output_ext = os.path.splitext(opts.output)

def interleave(x, y):
    ipix = 0
    i = 0
    while np.any(x) or np.any(y):
        ipix |= (x & 1) << i
        i += 1
        x >>= 1
        ipix |= (y & 1) << i
        i += 1
        y >>= 1
    return ipix

m = hp.read_map(fitsfilename, verbose=False)
npix = len(m)
nside = hp.npix2nside(npix)

# Normalize
m /= m.max()

for basepix in range(12):
    # Layout of PNG image:
    # data[N-1,0] data[N-1,1] data[N-1,2] ...  data[N-1, M-1]
    # ...         ...         ...         ...  ...
    # data[2, 0]  data[2, 1]  data[2, 2]  ...  data[2, M-1]
    # data[1, 0]  data[1, 1]  data[1, 2]  ...  data[1, M-1]
    # data[0, 0]  data[1, 0]  data[2, 0]  ...  data[0, M-1]

    y, x = np.mgrid[:nside, :nside]
    ipix_nest = nside * nside * basepix + interleave(x, y)
    ipix_ring = hp.nest2ring(nside, ipix_nest)
    data = np.clip(m[ipix_ring] * 255, 0, 255).astype(np.uint8)

    outfilename = '%s-%02d%s' % (output_name, basepix, output_ext)
    PIL.Image.fromarray(data, 'L').save(outfilename)
