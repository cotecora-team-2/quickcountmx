#!/usr/bin/python3
# usage:
# python3 scripts/monitor.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_results /home/rstudio/workspace/cotecora/mancera/compulsado_zac --wait_sec 5 --n_chains 4 --n_iter 300 --team ortizm --last True
# python3 scripts/monitor.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_results /home/rstudio/workspace/cotecora/mancera/compulsado_zac --wait_sec 5 --n_chains 4 --team ortizm --last True
# python3 scripts/monitor.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_results /home/rstudio/workspace/cotecora/mancera/compulsado_zac --wait_sec 5 --n_iter 300 --team ortizm --last True
# python3 scripts/monitor.py --data_path /home/rstudio/workspace/cotecora/unicom/cortes/zac --path_out /home/rstudio/workspace/cotecora/mancera/zac --path_results /home/rstudio/workspace/cotecora/mancera/compulsado_zac --wait_sec 5 --team ortizm --last True


import sys, getopt
import os
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
    print("Error procesando archivo")

  return descriptors

def main(params):
  if not os.path.exists(params.path_out):
    os.makedirs(params.path_out)
  if not os.path.exists(params.path_results):
    os.makedirs(params.path_results)
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
        if(descriptores["tipo"] == "REMESAS"):
          full_path = os.path.join(params.data_path, filename)
          subprocess.call(["r", "-e", "quickcountmx:::process_batch('" +full_path+"','"+descriptores['nombre']+"','"+params.path_out+"','"+params.path_results+"','"+params.team+"',n_chains='"+str(params.n_chains)+"',n_iter='"+str(params.n_iter)+"')"])
          subprocess.call(["sed","-i",'s/,,,,,$//g',params.path_results+'/compulsado'+descriptores['id_estado']+descriptores['fecha']+'.csv'])
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
    parser.add_argument("--path_results", "-pr", type=str,
                        help="Path of results")
    parser.add_argument('--n_chains', "-ch", type=int, default=4,
                        help="Number of chains for fit")
    parser.add_argument('--n_iter', "-it", type=int, default=300,
                        help="Number of chains for fit")
    parser.add_argument('--wait_sec', "-s", type=int,
                        help="Wait seconds")
    parser.add_argument('--team', "-t", type=str, default="default",
                        help="Team name")
    parser.add_argument('--last', '-l', type=bool, default=False,
                        help='whether it is the last one')

    params = parser.parse_args()

    main(params)
