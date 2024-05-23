data {
  int n_estratos;
  int p; // coaliciones
  int n; // tamaño de la muestra
  array[n,p] int votos;
  array[n] int estrato;
}

parameters {
  //matrix[p, n_estratos] alpha;
  array[n_estratos] vector<lower=0>[p] alpha;
}

model {
  for(i in 1:n){
    votos[i,] ~ dirichlet_multinomial(alpha[estrato[i]]);
  }
  for(s in 1:n_estratos){
    alpha[s] ~ exponential(0.05);
  }
}
