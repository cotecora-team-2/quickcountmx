#!/usr/bin/python3
# usage:
# python3 scripts/monitor.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True --even True --log_file chi.log
# python3 scripts/monitor.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True --even True
# python3 scripts/monitor.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True --log_file chi.log
# python3 scripts/monitor.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --wait_sec 5 --team ortizm --last True 

from settings import NUM_ITER, NUM_WARMUP, ADAPT_DELTA, MAX_TREEDEPTH, NUM_CHAINS, SEED
import sys, getopt
import os
import logging
import time
import subprocess
import argparse

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

def main(params):
  logging.basicConfig(filename=params.log_file, level=logging.DEBUG,
  format='%(levelname)s %(asctime)s %(message)s', datefmt='[%Y-%d-%m %H:%M:%S]')
  if not os.path.exists(params.path_out):
    os.makedirs(params.path_out)
  if not os.path.exists(params.path_mailbox):
    os.makedirs(params.path_mailbox)
  logging.info("Observando " + params.data_path + " cada "+ str(params.wait_sec) + " segundos.")
  files_before = [f for f in os.listdir(params.data_path) if f[:7] == "REMESAS"]
  files_before.sort()
  if boolarg_to_bool(params.last):
    files_before = files_before[:-1]
  while 1:
    files_now = [f for f in os.listdir(params.data_path) if f[:7] == "REMESAS"]
    added = [f for f in files_now if not f in files_before]
    if added:
      print(".")
      added.sort()
      print("Se agregaron: ", ",".join(added))
      # use last one even or not
      if len(added) >=1:
          if boolarg_to_num(params.even) > 0:
              added = [a for a in added if ((int(a[-5]) + boolarg_to_num(params.even)) % 2) == 1]
          added.sort()
      if added:
          added = added[-1:]
          for filename in added:
            descriptores = procesar_nombre(filename)
            logging.info(descriptores)
            logging.info("num_iter:{}, num_warmup:{}, adapt_delta:{}, max_treedepth:{}, num_chains:{}".format(NUM_ITER, NUM_WARMUP, ADAPT_DELTA, MAX_TREEDEPTH, NUM_CHAINS))
            if(descriptores["tipo"] == "REMESAS"):
              full_path = os.path.join(params.data_path, filename)
              infile = open(full_path, 'r')
              nrow = int(infile.readline())
              infile.close()
              logging.info('------------------------------------------------------')
              logging.info('*** Remesa: {}'.format(filename))
              logging.info("numero de casillas: {}".format(nrow))
              if nrow > 1:
                  subprocess.call(["r", "-e", "quickcountmx:::process_batch('" +full_path+"','"+descriptores['nombre']+"','"+params.log_file+"','"+params.path_out+"','"+params.path_mailbox+"','"+params.team+"','"+params.even+"',n_chains='"+str(NUM_CHAINS)+"',n_iter='"+str(NUM_ITER)+"',n_warmup='"+str(NUM_WARMUP)+"',adapt_delta='"+str(ADAPT_DELTA)+"',max_treedepth='"+str(MAX_TREEDEPTH)+"',seed='"+str(SEED)+"')"]) 
                  #subprocess.call(["sed","-i",'s/,,,,,$//g',params.path_results+'/compulsado'+descriptores['id_estado']+descriptores['fecha']+'.csv'])
              else:
                logging.info('no se estima con numero de casillas < 2')
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
                        help="A folder with this name would be created to dump saved models and log files")
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

    params = parser.parse_args()

    main(params)
