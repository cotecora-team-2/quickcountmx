import warnings
import os
import configparser
#import logging


class Init_vars():
    def __init__(self,edo_str,path,logging):
        self.edo_str = edo_str
        self.dotvars_path = path # './_vars'
        self.settings = {}
        self.vars_dict = {}
        self.INIT_INFO = {}
        self._parse_vars(logging)

    def _parse_vars(self,logging):
            try:
                config = configparser.ConfigParser()
                config.read(self.dotvars_path)

                _ = [self._init_dict_sec(sec) for sec in config.sections()]
                _ = [self._add_dict_items(sec,tp,True) for sec in config.sections() for tp in config.items(sec)]
 
                if 'default' in config.sections():
                    self.vars_dict = self.settings['default']
                if self.edo_str in config.sections():
                    _ = [self._add_dict_items(self.edo_str,k,False) for k in self.settings[self.edo_str]]
                
          
                self.INIT_INFO = {'NUM_ITER': int(self.vars_dict['num_iter']),
                        'NUM_WARMUP': int(self.vars_dict['num_warmup']),
                        'ADAPT_DELTA': float(self.vars_dict['adapt_delta']),
                        'MAX_TREEDEPTH': int(self.vars_dict['max_treedepth']),
                        'NUM_CHAINS': int(self.vars_dict['num_chains']),
                        'SEED': int(self.vars_dict['seed'])}
            except Exception as e:
                logging.warning(e)
                logging.warning('No valid configuration file found in {}. Prefixed init variables will be used.'.format(self.dotvars_path))
                self.INIT_INFO = {'NUM_ITER': 300,
                        'NUM_WARMUP': 200,
                        'ADAPT_DELTA': 0.8,
                        'MAX_TREEDEPTH': 10,
                        'NUM_CHAINS': 4,
                        'SEED': 221285}





    def _init_dict_sec(self,sec):
        self.settings[sec] = {}
        return True
  
    def _add_dict_items(self,sec,tp,is_settings):
        if is_settings:
            self.settings[sec][tp[0]] = tp[1] 
        else:
            self.vars_dict[tp] = self.settings[sec][tp]
        return True
