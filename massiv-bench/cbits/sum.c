
double c_sum(const double vec[], const int size){
  double sum = 0;
  for(int i = 0; i < size; i++){
    sum+= vec[i];
  }
  return sum;
}
