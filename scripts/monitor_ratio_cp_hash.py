#!/usr/bin/python3
# usage:
# python3 scripts/monitor_ratio_cp_hash.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/buzon2/razon/zac --wait_sec 5 --team ortizm --last True --b 100 --last_file _last_ratio_zac
# python3 scripts/monitor_ratio_cp_hash.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/buzon2/razon/zac --wait_sec 5 --team ortizm --last True --b 100

import sys, getopt
import os
import time
import subprocess
import argparse
from shutil import copy
import hashlib

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
    print("Error procesando archivo")

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
  if not os.path.exists(params.path_out):
    os.makedirs(params.path_out)
  if not os.path.exists(params.last_file):
    with open(params.last_file, "w") as myfile:
      myfile.write('0,,\n')
  print("Observando " + params.data_path + " cada "+ str(params.wait_sec) + " segundos.")
  files_before = [f for f in os.listdir(params.data_path) if f[:7] == "REMESAS"]
  files_before.sort()
  if params.last:
    files_before = files_before[:-1]
  while 1:
    files_now = [f for f in os.listdir(params.data_path) if f[:7] == "REMESAS"]
    added = [f for f in files_now if not f in files_before]
    if added:
      print(".")
      added.sort()
      print("Se agregaron: ", ",".join(added))
      # use last one
      added = added[-1:]
      for filename in added:
        descriptores = procesar_nombre(filename)
        print(descriptores)
        print("B:{}".format(params.b))
        if(descriptores["tipo"] == "REMESAS"):
          full_path = os.path.join(params.data_path, filename)
          infile = open(full_path, 'r')
          nrow = int(infile.readline().strip())
          infile.close()
          keep_trying = True
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
                         print("Remesa {} no valida!".format(last_full_path))
                         invalid = True
                         keep_trying = False
                     else:
                         last_hash = get_hash(params.last_file)
                         invalid = False
                  else:
                      last_hash = get_hash(last_full_path)
                      invalid = False
                  fn_out = 'razon' + descriptores['id_estado'] + descriptores['fecha'] + '.csv'
                  with open(params.last_file, "a") as myfile:
                      myfile.write('{},{},{}\n'.format(nrow,filename,fn_out))
                  keep_trying = False
              except Exception as e:
                  pass
          print(nrow)
          try:
              if nrow > 1:
                  if invalid:
                      print("Remesa {} no valida!".format(full_path))
                      raise StopIteration
                  if  get_hash(full_path) != last_hash:
                      subprocess.call(["r", "-e", "quickcountmx:::ratio_process_batch('" +full_path+"','"+descriptores['nombre']+"','"+params.path_out+"',B='"+str(params.b)+"','"+params.team+"')"]) 
                  else:
                      keep_trying = True
                      while keep_trying:
                          try:
                              last_full_out = os.path.join(params.path_out, last_fn_out)
                              full_out = os.path.join(params.path_out, fn_out)
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
                              keep_trying = False
                          except Exception as e:
                              time.sleep(int(params.wait_sec))
                              pass
                      print("Se copio {} como {}".format(last_fn_out,fn_out))
              else:
                print('no se estima con numero de casillas < 2')
          except StopIteration:
            pass
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
    parser.add_argument('--b', "-b", type=int, default=100,
                        help="Number of bootstrap replicates used to compute standard errors")
    parser.add_argument('--wait_sec', "-s", type=int,
                        help="Wait seconds")
    parser.add_argument('--team', "-t", type=str, default="default",
                        help="Team name")
    parser.add_argument('--last', '-l', type=bool, default=False,
                        help='whether it is the last one')
    parser.add_argument('--last_file', "-la", type=str, default="_last_ratio",
                        help="file where nrow and output filename are stored to be checked internally")

    params = parser.parse_args()

    main(params)