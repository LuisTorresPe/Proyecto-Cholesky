
#include<cstdlib>
#include <iostream>
#include <math.h>
#include <chrono>
#include<ctime>
#define HAVE_STRUCT_TIMESPEC
using namespace std;
using namespace std::chrono;
class matriz{//clase la cual hara todas las operaciones de las matrices
	private:
		int filas, columnas;//enviadas por el usuario al crear el objeto
		double **m;//este es la matriz dinamica, es un vector dinamico que apunta a otro vector dinamico como un apuntador de un apuntador
	public:
		matriz(int filas, int columnas);//creara la matriz de tama?o mxn
		~matriz();//destruye la matriz
		int cholesky();//Cholesky por pasos
		void print();//imprimira la matriz
		void formchol();//formula de cholesky
};
matriz::matriz(int _filas,int _columnas){//constructor de la matriz
	filas=_filas;//el dato filas enviado por el usuario se vuelve, el dato filas de la clase privada
	columnas=_columnas;//el dato columnas enviado por el usuario se vuelve el dato columnas de la clase privada
	m=new double*[filas];//se declara que m sera un apuntador o un arreglo dinamico hacia las filas
	for (int i=0;i<filas;i++){//recorrera las filas para colocar las columnas
		m[i]=new double[columnas];//se establece el donde se inicializa cada elemento de las columnas y designa el espacio de memoria
	}
	int elemento, des=0;
	cout<< "\n Si desea ingresar los datos presione el número 0, si desea que se generen aleatoriamente seleccione el 1 \n";
	cin>>des;
	if (des==0){
		for (int fi=0;fi<filas;fi++){//este ira avanzando por las filas
			for (int col=0;col<columnas;col++){//este ira avanzando por las columnas
			cout << "Ingrese el dato de la posicion a"<<fi<<","<<col<<"= \n";
			cin>>elemento;
				m[fi][col]=elemento;
			}
		}
	}
	if (des==1){
		for (int fi=0;fi<filas;fi++){//este ira avanzando por las filas
			for (int col=0;col<columnas;col++){//este ira avanzando por las columnas
				m[fi][col]=rand();//establece los numero aleatorios a la matriz
			}
		}
	}
	
}
matriz::~matriz(){//elimina los miembros de la matriz
    for (int i= 0; i < filas; i++){
        delete [] m[i];
    }
    delete [] m;
}
void matriz::print(){//imprime la matriz generada
		for (int fi=0;fi<filas;fi++){
		cout<< "\n";
		for (int col=0;col<columnas;col++){
			cout << m[fi][col]<<"\t";
		}
	}
}
int matriz::cholesky(){//Calculo de la funcion de cholesky por pasos 
	matriz cop(filas,columnas);//Matriz diagonal superior
	matriz rev(filas,columnas);
	int dia;//Este nos ayudara a estarnos moviendo en la diagonal
	double a,p1;//en a se asignara el pivote, mientras que p1 sera para hacer las operaciones basicas en las filas
	for(int fi=0;fi<columnas;fi++){//define mi matriz identidad la cual se convertira en la matriz inversa
		for(int col=0;col<columnas;col++){
			if(fi==col){
				rev.m[fi][col]=1;//asigna los uno en la diagonal
			}
			else{
				rev.m[fi][col]=0;//asigna 0 en las posiciones distintas a la diagonal
			}
			cop.m[fi][col]=m[fi][col];//copia la matriz original para no tener que modificarla
		}
	}
		
	for(int dia=0;dia<filas;dia++){
		a=cop.m[dia][dia];
		if (a!=0){
			for(int fi=0;fi<filas;fi++){//ira recorriendo las filas
				p1=cop.m[fi][dia]/a;//guardara el dato que este en la fila que se desea reducirla para poder reducirla
				if(fi>dia and p1!=0){//este ayudara a volver 0 los elementos de abajo y arriba del pivote
					for(int col=0;col<columnas;col++){
						cop.m[fi][col]=cop.m[fi][col]-p1*(cop.m[dia][col]);//efectua la reduccion 
					}	
					rev.m[fi][dia]=-p1;//coloca los elementos de la matriz diagonal inferior
				}
			}	
		}

	}
	//cout<<"\n La matriz diagonal inferior es\n";
	//rev.print();
	//cout<<"\n La matriz diagonal superior es \n";
	//cop.print();
	matriz fac(filas,columnas);//Matriz que factorizara la diagonal para ver si es de cholesky o no
	double cot,ra,modcol,k, comp=1, rec,b;
	cot=0;
	for (int dia=0;dia<filas;dia++){
		k=cop.m[dia][dia];//saca los elementos de la diagonal para factorizarlos
		if(k<=0){
			cot=1;
		}
	}
	if(cot==1){
		cout<<"\n No se podra calcular matriz de choleski \n";
	}
	if(cot==0){
		for(int fi=0;fi<filas;fi++){//Hace la factorizacion del numero en diagonal
			for(int col=0;col<columnas;col++){
				if(fi==col){
					ra=cop.m[fi][col];
					fac.m[fi][col]=ra;
					for(int modcol=fi;modcol<columnas;modcol++){
						cop.m[fi][modcol]=cop.m[fi][modcol]/ra;
					}	
				}
				else{
					fac.m[fi][col]=0;//asigna 0 en las posiciones distintas a la diagonal
				}
			}
		}
		comp=0;
		for(int fi=0;fi<filas;fi++){//muestra si obtenermos L y L transpuesta
			for (int col=0;col<columnas;col++){
				if(fac.m[fi][col]!=cop.m[col][fi]){
					comp=1;
				}
			}
		}
		if(comp=1){
			cout <<"\n No tiene matriz de cholesky \n";
		}
		if(comp=0){
			for(int dia=0;dia<filas;dia++){//Si es L y Ltranspuesta calcula la matriz de cholesky
				b=sqrt(fac.m[dia][dia]);
				for(int rec=dia;rec<columnas;rec++){
					cop.m[dia][rec]=b*cop.m[dia][rec];
					rev.m[rec][dia]=b*rev.m[rec][dia];
				}
			}
		}
	}
	return comp;
}
void matriz::formchol(){
	float sum;
	matriz rap(filas,columnas);
		for(int fi=0;fi<filas;fi++){
		for(int col=0;col<columnas;col++){
			if(fi==col){
				rap.m[fi][col]=1;
			}
			else{
				rap.m[fi][col]=0;
			}
		}
	}
	for(int fi=0;fi<filas;fi++){
		for(int col=0;col<columnas;col++){
			if(fi==col){//Para calcular los elementos de la diagonal
				if(col>0){
					sum=0;
					for(int pro=0;pro<col-1;fi++){
						sum=sum+pow(rap.m[fi][pro],2);
					}
				}
				rap.m[fi][col]=sqrt(m[fi][col]-sum);//se asigna el elemento a la diagonal
			}
			if(fi>col){//los elementos de la matriz, para generar la diagonal inferior
				if(col>0){
					sum=0;
					for(int pro=0;pro<col-1;col++){
						sum=sum+rap.m[col][pro]*rap.m[fi][pro];
					}
					rap.m[fi][col]=(m[fi][col]-sum)/(rap.m[col][col]);//asignacion del elemento
				}
			}
		}
	}
	//rap.print();//Matriz de cholesky dada por formula
}
int main(){
	srand(time(NULL));//Nos ayuda a generar elementos completamente aleatorios
	int fila,k;
	cout<<"\n Ingrese el tana?o de la matriz cuadrada en la que desea hacer la operacion \n";
	cin>>fila;//El tama?o de la matriz cuadrada
	matriz m1(fila,fila);//genera el objeto m1 de la clase matriz
	//cout<<"\n La matriz es: \n";
	//m1.print();
	auto start = high_resolution_clock::now();//comienza a contar el tiempo que tardara el proceso LU
	k=m1.cholesky();
	auto stop = high_resolution_clock::now();//Acaba de contar el tiempo del proces
	auto duration =duration_cast<microseconds>(stop-start);//Calcula la duracion del proceso
	cout << "Tiempo el metodo de factorizacion mediante LU: " << (double)duration.count()/1e6 << endl;
	if (k==0){
		cout<< "Ahora se hara la reduccion de cholesky y se compararan los tiempos de los dos procesos";
		auto start = high_resolution_clock::now();//Comienza a contar el proceso de cholesky
		m1.formchol();//Del obejto 1, calcula la matriz de cholesky por formula
		auto stop = high_resolution_clock::now();//detiene el tiempo de duracion de cholesky
		auto duration =duration_cast<microseconds>(stop-start);//Calcula la duracion de este
		cout << "Tiempo durante el uso de la formula: " << (double)duration.count()/1e6 << endl;
	}
	if(k==1){
		cout<<"\n No tendra reduccion de cholesky";
	}
	return(0);
}
