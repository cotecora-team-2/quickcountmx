#!/usr/bin/python3
# usage:
# python3 scripts/monitor_cp_hash.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True --even True --log_file zac.log --last_file _last_zac
# python3 scripts/monitor_cp_hash.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True --even True --log_file zac.log
# python3 scripts/monitor_cp_hash.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True --even True
# python3 scripts/monitor_cp_hash.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True --log_file chi.log
# python3 scripts/monitor_cp_hash.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True 

from settings import Init_vars
import sys, getopt
import os
import logging
import time
import subprocess
import argparse
from shutil import copy
import hashlib

def boolarg_to_num(x):
  if x in ["TRUE","True","true"]:
    logging.info("Se procesaran las remesas par.")
    print("Se procesaran todas las remesas par.")
    return 1
  elif x in ["FALSE","False","false"]:
    logging.info("Se procesaran todas las remesas non.")
    print("Se procesaran todas las remesas non.")
    return 2
  else:
    logging.info("Se procesaran todas las remesas.")
    print("Se procesaran todas las remesas.")
    return 0

def boolarg_to_bool(x):
  if x in ["TRUE","True","true"]:
    return True
  elif x in ["FALSE","False","false"]:
    return False
  else:
    logging.info("Se procesara la remesa final.")
    print("Se procesara la remesa final.")
    return True


def procesar_nombre(filename):
  descriptors = {'tipo':'Desconocido'}
  try:
    nombre, ext = filename.split(".")
    if(ext=='txt'):
      codigo = nombre[-10:]
      tipo = nombre[:-10]
      eleccion_tipo = codigo[:2]
      id_estado = codigo[2:4]
      fecha = codigo[4:10]
      descriptors = {'nombre':nombre, 'tipo':tipo,
          'eleccion_tipo':eleccion_tipo,
          'id_estado':id_estado,
          'fecha':fecha}
  except Exception as ex:
    logging.warning("Error procesando archivo")

  return descriptors

def get_hash(filename):
    sha1 = hashlib.sha1()
    with open(filename, 'rb') as f:
        while True:
            data = f.read()
            if not data:
                break
            sha1.update(data)

    return sha1.hexdigest()

def main(params):
  keep_trying = True
  logging.basicConfig(filename=params.log_file, level=logging.DEBUG,
  format='%(levelname)s %(asctime)s %(message)s', datefmt='[%Y-%d-%m %H:%M:%S]')
  if not os.path.exists(params.path_out):
    os.makedirs(params.path_out)
  if not os.path.exists(params.path_mailbox):
    os.makedirs(params.path_mailbox)
  npath_mailbox = '/'.join(params.path_mailbox.split('/')[:-2]) + '/pctpropobs'
  if not os.path.exists(npath_mailbox):
    os.makedirs(npath_mailbox)
  if not os.path.exists(params.last_file):
    with open(params.last_file, "w") as myfile:
      myfile.write('0,,\n')
  logging.info("Observando " + params.data_path + " cada "+ str(params.wait_sec) + " segundos.")
  files_before = [f for f in os.listdir(params.data_path) if f[:7] == "REMESAS"]
  files_before.sort()
  if boolarg_to_bool(params.last):
    files_before = files_before[:-1]
  is_even = boolarg_to_num(params.even)
  while 1:
    files_now = [f for f in os.listdir(params.data_path) if f[:7] == "REMESAS"]
    added = [f for f in files_now if not f in files_before]
    if added:
      print(".")
      added.sort()
      logging.info('======================================================')
      logging.info("Se agregaron: {}".format(",".join(added)))
      print("Se agregaron: ", ",".join(added))
      time.sleep(2.0)
      # use last one even or not
      if len(added) >=1:
          if is_even > 0:
              added = [a for a in added if ((int(a[-5]) + is_even) % 2) == 1]
          added.sort()
      if added:
          added = added[-1:]
          for filename in added:
            descriptores = procesar_nombre(filename)
            init_info = Init_vars(descriptores['id_estado'],'_vars',logging)
            logging.info(descriptores)
            logging.info(init_info.INIT_INFO)
            if(descriptores["tipo"] == "REMESAS"):
              full_path = os.path.join(params.data_path, filename)
              infile = open(full_path, 'r')
              nrow = int(infile.readline().strip())
              infile.close()
              keep_trying = True
              invalid = False
              while keep_trying:
                  last_fn_out = ""
                  try:
                      with open(params.last_file, 'r') as infile:
                          last_nrow_file = infile.readlines()
                      last_nrow = int(last_nrow_file[-1].split(',')[0])
                      last_filename = last_nrow_file[-1].strip().split(',')[1]
                      last_fn_out = last_nrow_file[-1].strip().split(',')[2]
                      last_full_path = os.path.join(params.data_path, last_filename)
                      if last_full_path[-1] == '/':
                          if last_nrow != 0:
                              logging.info("Remesa {} no valida!".format(last_full_path))
                              invalid = True
                              keep_trying = False
                          else:
                              last_hash = get_hash(params.last_file)
                              invalid = False
                      else:
                          last_hash = get_hash(last_full_path)
                          invalid = False
                      fn_out = params.team + descriptores['id_estado'] + descriptores['fecha'] + '.csv'
                      with open(params.last_file, "a") as myfile:
                          myfile.write('{},{},{}\n'.format(nrow,filename,fn_out))
                      keep_trying = False
                  except Exception as e:
                      pass
              
              logging.info('------------------------------------------------------')
              logging.info('*** Remesa: {}'.format(filename))
              logging.info("numero de casillas: {}".format(nrow))
              try:
                  if nrow > 1:
                      if invalid:
                          logging.info("Remesa {} no valida!".format(full_path))
                          raise ValueError("Remesa {} no valida!".format(full_path))
                      if  get_hash(full_path) != last_hash:
                          subprocess.call(["r", "-e", "quickcountmx:::process_batch('" +full_path+"','"+descriptores['nombre']+"','"+params.log_file+"','"+params.path_out+"','"+params.path_mailbox+"','"+params.team+"','"+params.even+"',n_chains='"+str(init_info.INIT_INFO['NUM_CHAINS'])+"',n_iter='"+str(init_info.INIT_INFO['NUM_ITER'])+"',n_warmup='"+str(init_info.INIT_INFO['NUM_WARMUP'])+"',adapt_delta='"+str(init_info.INIT_INFO['ADAPT_DELTA'])+"',max_treedepth='"+str(init_info.INIT_INFO['MAX_TREEDEPTH'])+"',seed='"+str(init_info.INIT_INFO['SEED'])+"')"]) 
                          #subprocess.call(["sed","-i",'s/,,,,,$//g',params.path_results+'/compulsado'+descriptores['id_estado']+descriptores['fecha']+'.csv'])
                      else:
                          keep_trying = True
                          while keep_trying:
                              try:
                                  last_full_out = os.path.join(params.path_out, last_fn_out)
                                  full_out = os.path.join(params.path_out, fn_out)
                                  full_mailbox = os.path.join(params.path_mailbox, fn_out)
                                  with open(last_full_out, 'r') as infile:
                                      last_results_list = infile.readlines()
                                  with open(full_out, "w") as myfile:
                                      for line in last_results_list:
                                          new_line = line.split(',')
                                          if new_line[2] == 'R':
                                              myfile.write(line)
                                          else:
                                              new_line[2] = descriptores['fecha']
                                              myfile.write(','.join(new_line))
                                  copy(full_out,full_mailbox)
                                  keep_trying = False
                              except Exception as e:
                                  time.sleep(int(params.wait_sec))
                                  pass
                          logging.info("Se copio {} como {}".format(last_fn_out,fn_out))
                  else:
                    logging.info('no se estima con numero de casillas < 2')
              except Exception as e:
                pass
      else:
        print('.', end = '', flush = True)
    else:
      print('.', end = '', flush = True)
    files_before = files_now
    time.sleep(int(params.wait_sec))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Quickcountmx monitor')

    # Experiment setup params
    parser.add_argument("--data_path", "-dp", type=str, default="default1",
                        help="Data path of input")
    parser.add_argument("--path_out", "-po", type=str,
                        help="Data path of output")
    parser.add_argument("--path_mailbox", "-pm", type=str,
                        help="Data path of output in second directory")
    parser.add_argument('--wait_sec', "-s", type=int,
                        help="Wait seconds")
    parser.add_argument('--team', "-t", type=str, default="default",
                        help="Team name")
    parser.add_argument('--last', '-l', type=str, default="True",
                        help='whether it is the last one')
    parser.add_argument('--even', '-e', type=str, default="0",
                        help='whether it estimates even times')
    parser.add_argument('--log_file', '-lf', type=str, default="log",
                        help='log filename')
    parser.add_argument('--last_file', '-la', type=str, default="_last",
                        help="file where nrow and output filename are stored to be checked internally")

    params = parser.parse_args()

    main(params)
