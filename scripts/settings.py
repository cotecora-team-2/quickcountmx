import warnings
import os

dotvars_path = './.vars'

if os.path.isfile(dotvars_path):
    with open(dotvars_path, 'r') as f:
        vars_dict = dict(
            tuple(line.split('='))
            for line in f.readlines() if not line.startswith('#')
        )
else:
    warnings.warn('No configuration file found in %s' % dotvars_path)

NUM_ITER=int(vars_dict['num_iter'])
NUM_WARMUP=int(vars_dict['num_warmup'])
ADAPT_DELTA=float(vars_dict['adapt_delta'])
MAX_TREEDEPTH=int(vars_dict['max_treedepth'])
NUM_CHAINS=int(vars_dict['num_chains'])
