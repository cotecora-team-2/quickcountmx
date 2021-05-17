#!/usr/bin/python3
# usage:
# python3 scripts/process_batch.py --abs_filename /home/rstudio/workspace/cotecora/unicom/cortes/zac/REMESAS0232291840.txt --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --team ortizm --log_file zac.log --vars_file _vars
# python3 scripts/process_batch.py --abs_filename /home/rstudio/workspace/cotecora/unicom/cortes/zac/REMESAS0232291840.txt --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --team ortizm --log_file zac.log
# python3 scripts/process_batch.py --abs_filename /home/rstudio/workspace/cotecora/unicom/cortes/zac/REMESAS0232291840.txt --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --team ortizm --vars_file _vars
# python3 scripts/process_batch.py --abs_filename /home/rstudio/workspace/cotecora/unicom/cortes/zac/REMESAS0232291840.txt --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_mailbox /home/rstudio/workspace/cotecora/buzon2/estimaciones/zac --team ortizm

from settings import Init_vars
import sys, getopt
import os
import logging
import time
import subprocess
import argparse


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
  logging.info('======================================================')
  logging.info("Estimando " + params.abs_filename)
  filename = os.path.basename(params.abs_filename)
  descriptores = procesar_nombre(filename)
  init_info = Init_vars(descriptores['id_estado'],params.vars_file,logging) # './_vars'
  logging.info(descriptores)
  logging.info(init_info.INIT_INFO)
  if(descriptores["tipo"] == "REMESAS"):
    infile = open(params.abs_filename, 'r')
    nrow = int(infile.readline())
    infile.close()
    logging.info('------------------------------------------------------')
    logging.info('*** Remesa: {}'.format(filename))
    logging.info("numero de casillas: {}".format(nrow))
    if nrow > 1:
        subprocess.call(["r", "-e", "quickcountmx:::process_batch('" +params.abs_filename+"','"+descriptores['nombre']+"','"+params.log_file+"','"+params.path_out+"','"+params.path_mailbox+"','"+params.team+"',n_chains='"+str(init_info.INIT_INFO['NUM_CHAINS'])+"',n_iter='"+str(init_info.INIT_INFO['NUM_ITER'])+"',n_warmup='"+str(init_info.INIT_INFO['NUM_WARMUP'])+"',adapt_delta='"+str(init_info.INIT_INFO['ADAPT_DELTA'])+"',max_treedepth='"+str(init_info.INIT_INFO['MAX_TREEDEPTH'])+"',seed='"+str(init_info.INIT_INFO['SEED'])+"')"]) 
    else:
        logging.info('no se estima con numero de casillas < 2')

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Quickcountmx monitor')

    # Experiment setup params
    parser.add_argument("--abs_filename", "-af", type=str, default="default1",
                        help="A full input file")
    parser.add_argument("--path_out", "-po", type=str,
                        help="Data path of output")
    parser.add_argument("--path_mailbox", "-pm", type=str,
                        help="Data path of output in second directory")
    parser.add_argument('--team', "-t", type=str, default="default",
                        help="Team name")
    parser.add_argument('--log_file', '-lf', type=str, default="log",
                        help='log filename')
    parser.add_argument('--vars_file', '-vf', type=str, default="_vars",
                        help='vars filename')

    params = parser.parse_args()

    main(params)
