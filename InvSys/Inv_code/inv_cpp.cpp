#include <cstdlib>
#include <cassert>
#include <sstream>
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;
#include "svd.h"
//Coded by Feng Deng, University of Toronto
//for internal use only
struct inout{
	string path;
	string ftransms;
	string fpriors;
	string fqmats;
	string co2fs;
	string varfs;
	string fluxfs;
	string uncerts;
	int m_r;
	int n_c;
	double lamda;
	double qscaler;
};


int add_(int m, int n, double *w){
	int i;
	double ep = pow(2.0,-52.0);
	double tl = max(m,n)*w[0]*ep;
	int r = 0;
	for (i = 0; i < n; i++) {
		if (w[i] > tl) {
			r++;
        }
    }
	return r;
}

int main(){
	inout iostr;
	iostr.path="./";
	iostr.ftransms="##FTRANSMS##";
	iostr.fpriors="##FPRIORS##";
	iostr.fqmats="##FQMATS##";
	iostr.co2fs="##CO2FS##";
	iostr.varfs="##VARFS##";
	iostr.fluxfs="##FLUXFS##";
	iostr.uncerts="##UNCERTS##";
	iostr.m_r=##M##;
	iostr.n_c=##N##;
	iostr.lamda=1;
	iostr.qscaler=1.;
	int n=iostr.n_c;
	int m=iostr.m_r;
	if(n>m){
		cout<<"You cannot use this code to solve your problem!"<<endl;
		exit(1);
	}

	double **resp, *xcov, *x0, *bcov, *bco2;
	int i, j;
	resp=(double **)malloc(m*sizeof(double *));
	if (NULL==resp){
		free(resp);
		cout<<"Memory allocation failed while allocating for resp[]"<<endl;; 
		exit(-1);
	}
	for(i=0; i<m; i++){
		resp[i]=(double *)malloc(n*sizeof(double));
		if(NULL==resp[i]){
			free(resp[i]);
			cout<<"Memory allocation failed while allocating for resp[i]"<<endl;;
			exit(-1);
		}
	}

	xcov=(double*)malloc(n*sizeof(double));
	if(NULL==xcov){
		free(xcov);
		cout<<"Memory allocation failed while allocating for xcov"<<endl;; 
		exit(-1);
	}

	x0=(double*)malloc(n*sizeof(double));
	if(NULL==x0){
		free(x0);
		cout<<"Memory allocation failed while allocating for x0"<<endl;; 
		exit(-1);
	}

	bcov=(double*)malloc(m*sizeof(double));
	if(NULL==bcov){
		free(bcov);
		cout<<"Memory allocation failed while allocating for bcov"<<endl;; 
		exit(-1);
	}

	bco2=(double*)malloc(m*sizeof(double));
	if(NULL==bco2){
		free(bco2);
		cout<<"Memory allocation failed while allocating for bco2"<<endl;; 
		exit(-1);
	}
        
	ifstream rm((iostr.path+iostr.ftransms).c_str(),ios::in|ios::binary);
	float tempr;
	if (rm){
		for(i=0; i<m; i++){
			for(j=0; j<n; j++){
				rm.read((char*)&tempr,sizeof(float));
				resp[i][j]=tempr;
				}
		}
		string s_m="matrix.txt";
		ofstream s_f1(s_m.c_str(), ios::out);
		for(i=0; i<m; i++){
			for(j=0; j<n; j++){
				s_f1<<resp[i][j];
				if (j==n-1){
					s_f1<<endl;
				}
			}
		}
		s_f1.close();
		rm.close();
	}else{
		cout<<"cannot open the response matrix file: "<<iostr.path+iostr.ftransms<<endl;
		exit(1);
	}

	ifstream xcovin((iostr.path+iostr.fqmats).c_str(), ios::in);
	if (xcovin){
		float tempx;
		for(i=0; i<n; i++){
			xcovin>>tempx;		
			xcov[i]=iostr.qscaler*tempx;
		}
		xcovin.close();
	}else{
		cout<<"cannot open the apriori covariance file: "<<iostr.path<<iostr.fqmats<<endl;
		exit(1);
	}
	

	ifstream inico2f((iostr.path+iostr.fpriors).c_str(),ios::in);
	if (inico2f){
		for(i=0; i<n; i++){
			inico2f>>x0[i];
		}
		inico2f.close();
	}else{
		cout<<"cannot open apriori input file: "<<iostr.path+iostr.fpriors<<endl;
		exit(1);
	}

	ifstream bcovin((iostr.path+iostr.varfs).c_str(),ios::in);
	if (bcovin){
		float tempb;
		for(i=0; i<m; i++){
			bcovin>>tempb;
			bcov[i]=sqrt(tempb);
		}
		bcovin.close();
	}else{
		cout<<"cannot open the measurement uncertainty covariance file: "<<iostr.path+iostr.varfs<<endl;
		exit(1);
	}

	ifstream bco2in((iostr.path+iostr.co2fs).c_str(),ios::in);
	if (bco2in){
		for(i=0; i<m; i++){
			bco2in>>bco2[i];
			}
		bco2in.close();
	}else{
		cout<<"cannot open co2 concentration data input file: "<<iostr.path+iostr.co2fs<<endl;
		exit(1);
	}
	
	for(i=0; i<m; i++){
		double tmp_a=0;
		for(j=0; j<n; j++){
			tmp_a=tmp_a+resp[i][j]*x0[j];
		}
		bco2[i]=(bco2[i]-tmp_a)/bcov[i];
	}
	for(i=0; i<m; i++){
		for(j=0; j<n; j++){
			resp[i][j]=resp[i][j]/bcov[i];
		}
	}
	for(i=0; i<m; i++){
		for(j=0; j<n; j++){
			resp[i][j]=resp[i][j]*xcov[j];
		}
	}

	double *w;
	w=(double *)malloc(n*sizeof(double));
	if(NULL==w){
		free(w);
		cout<<"Memory allocation failed while allocating for w"<<endl;; 
		exit(-1);
	}

	double **v;

	v=(double **)malloc(n*sizeof(double *));
	if (NULL==v){
		free(v);
		cout<<"Memory allocation failed while allocating for resp[]"<<endl; 
		exit(-1);
	}
	for(i=0; i<n; i++){
		v[i]=(double *)malloc(n*sizeof(double));
		if(NULL==v[i]){
			free(v[i]);
			cout<<"Memory allocation failed while allocating for resp[i]"<<endl;
			exit(-1);
		}
	}
	
	compute_svd(resp, m, n, w, v);
	svd_sort(resp, w, v, n, m);
	int r=add_(m, n, w);
	cout<<"Rank is "<<r<<endl;
	string s_fn="sfile.txt";
	ofstream s_f(s_fn.c_str(), ios::out); 
	for(int ii=0; ii<n; ii++){
		s_f<<w[ii]<<endl; 
	}
	s_f.close();

	ofstream xfile((iostr.path+iostr.fluxfs).c_str(), ios::out);    
    int k;
	for(i=0; i<n; i++){
		double xm=0;
		for(j=0; j<n; j++){
			double utb=0;
			for(k=0; k<m; k++){
				utb=utb+resp[k][j]*bco2[k];
			}
			xm=xm+w[j]/(w[j]*w[j]+1)*utb*v[i][j];
		}
		xm=xm*xcov[i]+x0[i];
		xfile<<xm<<" ";
		if((i+1)%69==0){
			xfile<<endl;
		}
		
	}
	xfile.close();
        ofstream xfile1((iostr.path+iostr.uncerts).c_str(), ios::out);
        for(i=0; i<n; i++){
            for(k=0; k<n; k++){
                double xm=0;
                for(j=0; j<n; j++){
                xm=xm+xcov[i]*xcov[k]*v[i][j]*v[k][j]/(w[j]*w[j]+1);
            }
            xfile1<<xm<<" ";
        }
        xfile1<<endl;
        }
        xfile1.close();
        return (0);
}

