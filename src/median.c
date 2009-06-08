#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <limits.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))


static void mmedian3(int *seq,int *slen,int *res)
{
        int len,i,cp,wn[3];
	len = *slen;
        wn[0] = seq[0];
        wn[1] = seq[1];
	cp = 2;
        for (i=0; i<len-2; i++)
        {
                wn[cp] = seq[i+2];
                if (wn[0] <= wn[1])
                {
                        if (wn[1] <= wn[2])
                        {
                                res[i] = wn[1];
                        }
                        else {
                                if (wn[0] <= wn[2])
                                        res[i] = wn[2];
                                else
                                        res[i] = wn[0];
                        }
                }
                else {
                        if (wn[1] <= wn[2])
                        {
                                if( wn[0] <= wn[2])
                                        res[i] = wn[0];
                                else
                                        res[i] = wn[2];
                        }
                        else
                        {
                                res[i] = wn[1];
                        }
                }
		cp++;
                cp = cp % 3;
        }

}

static int cmp(const void *a,const void *b)
{
        return *((int*)a)-*((int*)b);
}

static int find(int *s,int len,int v)
{
        int min,mid,max;
        min = 0;
        max = len-1;
	if (s[min] >= v) return(min);
	if (s[max] <= v) return(max);
	mid = (min+max)/2;
	while (min != mid)
	{
		if (s[mid] == v) return mid;
		if (s[mid] < v) min = mid; else max = mid;
		mid = (min+max)/2;
	}
	return min;
}

static void table_mmedian(int *seq,int *slen,int* wsize,int *res)
{
	int len,w,i,midx,cur_median,fcnt,cnt,av,dv,*marker; 
	w = *wsize;
	len = *slen;
	midx = (w+1)/2;
	marker = malloc(sizeof(int)*len);
	for (i=0; i< len; i++)
		marker[i] = 0;
	for (i=0; i< w; i++)
		marker[seq[i]] ++;
	cur_median = -1;
	fcnt = 0;
	while (fcnt < midx) 
		fcnt+=marker[++cur_median];
	for (cnt = 0; cnt < len-w+1; cnt++)
	{
		res[cnt] = cur_median; 
		if (cnt < len-w) {
			dv = seq[cnt];
			av = seq[cnt+w];
			if (av <= cur_median) fcnt ++;
			if (dv <= cur_median) fcnt --;
			marker[dv]--;
			marker[av]++;
			while (fcnt < midx) 
				 fcnt += marker[++cur_median];
			while (fcnt-marker[cur_median]  >= midx) 
				fcnt -= marker[cur_median--];
		}
	}
	free(marker);
}


void heap_mmedian(int *seq,int *slen, int *wsize,int *res)
{
	int len,w,i,*order,midx,cnt,ridx,iidx,nv;
	w = *wsize;
	len = *slen;
	midx = (w-1)/2;
	order = malloc(sizeof(int)*w);
	for (i=0; i < w; i++)
		order[i] = seq[i];
	qsort(order,w,sizeof(int),cmp);
	for (cnt = 0; cnt < len-w+1; cnt++)
	{
		res[cnt] = order[midx];
		if (cnt < len-w) {
			ridx=find(order,w,seq[cnt]);
			nv = seq[cnt+w];
			iidx = find(order,w,nv);
			if (ridx < iidx) 
				memmove(order+ridx,order+ridx+1,sizeof(int)*(iidx-ridx+1));
			if (ridx > iidx) 
			{
				memmove(order+iidx+1,order+iidx,sizeof(int)*(ridx-iidx));
				if (order[iidx] < nv) iidx++;
			}
			order[iidx] = nv;
		}
	}
	free(order);
}


void mmedian(int *seq,int *slen,int* wsize,int *res)
{
	if (*wsize == 3) 
		mmedian3(seq,slen,res);
	else  if (*wsize < 0.001**slen)  
		heap_mmedian(seq,slen,wsize,res); 
	else 
		table_mmedian(seq,slen,wsize,res); 
}


void table_mmedian_c(int *seq,int *slen,int* wsize,int *res)
{
	int len,w,i,midx,cur_median,fcnt,cnt,av,dv,*marker; 
	w = *wsize;
	len = *slen;
	midx = (w+1)/2;
	marker = malloc(sizeof(int)*len);
	for (i=0; i< len; i++)
		marker[i] = 0;
	for (i=0; i< w; i++)
		marker[seq[(len+i-midx+1)%len]] ++;
	cur_median = -1;
	fcnt = 0;
	while (fcnt < midx) 
		fcnt+=marker[++cur_median];
	for (cnt = 0; cnt < len; cnt++)
	{
		res[cnt] = cur_median; 
		if (cnt < len-1) {
			dv = seq[(len+cnt-midx+1) % len];
			av = seq[(len+cnt+midx)%len];
			if (av <= cur_median) fcnt ++;
			if (dv <= cur_median) fcnt --;
			marker[dv]--;
			marker[av]++;
			while (fcnt < midx) 
				 fcnt += marker[++cur_median];
			while (fcnt-marker[cur_median]  >= midx) 
				fcnt -= marker[cur_median--];
		}
	}
	free(marker);
}

void heap_mmedian_c(int *seq,int *slen,int* wsize,int *res)
{
        int len,w,i,*order,midx,cnt,ridx,iidx,nv;
        w = *wsize;
        len = *slen;
        midx = (w-1)/2;
        order = malloc(sizeof(int)*w);
        for (i=0; i < w; i++)
                order[i] = seq[(len+i-midx) % len];
        qsort(order,w,sizeof(int),cmp);
        for (cnt = 0; cnt < len; cnt++)
        {
                res[cnt] = order[midx];
                ridx=find(order,w,seq[(len+cnt-midx)%len]);
                nv = seq[(cnt+midx+1)%len];
                iidx = find(order,w,nv);
                if (ridx < iidx)
                        memmove(order+ridx,order+ridx+1,sizeof(int)*(iidx-ridx+1));
                if (ridx > iidx)
                {
                        memmove(order+iidx+1,order+iidx,sizeof(int)*(ridx-iidx));
                        if (order[iidx] < nv) iidx++;
                }
                order[iidx] = nv;
        }
}



void mmedian_c(int *seq,int *slen,int* wsize,int *res)
{
	if (*wsize < 0.001**slen)  
		heap_mmedian_c(seq,slen,wsize,res); 
	else 
		table_mmedian_c(seq,slen,wsize,res); 
}

#define NMOD(v,m) ((((v)%(m))<0)?(m)+((v)%(m)):((v)%(m)))
#define NDIV(v,m) (((v)<0)?(((v)-(m)+1)/(m)):(v)/(m))


static int *lsseq;
static int g_smedian2_start; 
static int g_smedian2_slen; 
static int mcmp2(const void *a,const void *b) 
{ 
	int i1,i2; 
	i1 = *((int*)a); 
	i2 = *((int*)b); 
	return lsseq[NMOD(g_smedian2_start+i1,g_smedian2_slen)]-lsseq[NMOD(g_smedian2_start+i2,g_smedian2_slen)]; 
}

static int mcmp(const void *a,const void *b) 
{ 
	return lsseq[*((int*)a)]-lsseq[*((int*)b)]; 
}

static double smedian2(int *s,int start,int len, int slen)
{
	int i,*mcont;
	double med;
	if (len == 0) return -1;
	if (len == 1) return *(s+NMOD(start,slen));
	if (len == 2) return (*(s+NMOD(start,slen)) +*(s+NMOD(start+1,slen)))/2;
	mcont = malloc(sizeof(int)*len);
	for (i=0; i<len; i++) mcont[i] =i;
	lsseq=s;
	g_smedian2_start = start;
	g_smedian2_slen = slen;
	qsort(mcont,len,sizeof(int),mcmp2);
	if (len %2)
		med= s[NMOD(start+mcont[len/2],slen)];
	else
		med= (double)(s[NMOD(start+mcont[len/2-1],slen)]+s[NMOD(start+mcont[len/2],slen)])/(double)2.0;
	free(mcont);
	return med;
}

static double smedian(int *s,int len)
{
	int i,*mcont;
	double med;
	if (len == 0) return -1;
	if (len == 1) return *s;
	if (len == 2) return (*s +*(s+1))/2;
	mcont = malloc(sizeof(int)*len);
	for (i=0; i<len; i++) mcont[i] =i;
	lsseq=s;
	qsort(mcont,len,sizeof(int),mcmp);
	if (len %2)
		med= s[mcont[len/2]];
	else
		med= (double)(s[mcont[len/2-1]]+s[mcont[len/2]])/(double)2.0;
	free(mcont);
	return med;
}



static double *ltab = NULL;

static void Genltab(int N)
{
	int i;
	ltab = malloc(sizeof(double)*(N+1));
	ltab[0] = ltab[1] = 0;
	for (i=2; i <= N; i++)
		ltab[i] = ltab[i-1]+log(i);
}


#define LCHOOSE(N,K) ( (K==0)?0:(ltab[(int)(N)]  -  ((ltab[(int)(K)]) + (ltab[(int)(N-K)])) ))


static double CalcPval(int N,int W,double r)
{
	int k,r_dwn,r_up,d,dst,tp;
	double frac,ltperm,p=0; //,tp;
	if (MIN((r-1),(N-r)) >=  (double)(W-1)/2)
	{
		k = (W-1)/2;
		r_dwn = (int)r;
		r_up  = (int)(r+0.75);
		if (W % 2) 
		{
			if (r_dwn == r_up) 
		                p = exp(LCHOOSE(r-1,k)+LCHOOSE(N-r,k)-LCHOOSE(N,W));
		}
		else
		{
			ltperm = LCHOOSE(N,W);
			tp = MIN(r_dwn-k-1,N-r_up-k);
			dst = 1-r_up+r_dwn;
			for (d=dst; d<= tp; d++)
				p = p + exp(LCHOOSE((r_dwn-1-d),k)+LCHOOSE((N-r_up-d),k)-ltperm);
		}
	}
	return p;
}

static void CumPvalTB(int N, int W, double r, double *tv,double *bv)
{
	double cnt = 0,p=0;
	if (W == 0)
	{
		*tv = 2.0; *bv=2.0;
		return;
	}
	if (W >= N)
	{
		*tv = 1.0; *bv=1.0;
		return;
	}

	if ( r > (double)N/2.) {
		for (cnt= r+0.5; cnt <= N; cnt+=0.5)
			p = p+CalcPval(N,W,cnt);
		*tv= p+CalcPval(N,W,r); 
		*bv = 1.0-p;
	} else {
		for (cnt= 0; cnt < r; cnt+=0.5)
			p = p+CalcPval(N,W,cnt);
		*tv = 1.0 -p;
		*bv = p+CalcPval(N,W,r);
	}
}




static double CumPval(int N, int W, double r, int top)
{
	double cnt = 0,p=0;
	if (W == 0)
		return 2;
	if (top) {
		if ( r > (double)N/2.) {
			for (cnt= r; cnt <= N; cnt+=0.5)
				p = p+CalcPval(N,W,cnt);
		} else {
			for (cnt= 0; cnt < r; cnt+=0.5)
				p = p+CalcPval(N,W,cnt);
			p = 1.0-p;
		}
	
	}
	else
	{
		if (r > (double)N/2) {
			for (cnt= r+0.5; cnt <= N; cnt+=0.5)
				p = p+CalcPval(N,W,cnt);
			p = 1-p;
		} else { 
			for (cnt= 0; cnt <= r; cnt+=0.5)
				p = p+CalcPval(N,W,cnt);
		}
	}
	return p;
}

void pos_pmedian(int *seq,double *pos,double *bounds,int *slen,double* wsize,int *rres,double *pvalsr,double *pvalsa)
{
	double step,cp;
	int start,stop,i;
	Genltab(*slen);
	step = (bounds[1]-bounds[0])/(*rres);
	start = 0;
	stop  = 0;
	cp = bounds[0]+step/2;
	for (i=0; i<(*rres); i++)
	{
		while((pos[start] < cp-*wsize/2) && (start < *slen)) 
			start++;
		while((pos[stop] < cp+*wsize/2) && (stop < *slen)) 
			stop++;
		if ((cp-*wsize/2 >= bounds[0]) && (cp+*wsize/2) <= bounds[1]) 
		{
			CumPvalTB(*slen, stop-start,smedian(seq+start,stop-start), pvalsr+i,pvalsa+i);
		}
		else
		{
				pvalsr[i] = 1;
				pvalsa[i] = 1;
		}
		cp += step;
	}
	free(ltab);
}

#if 0
void pos_mmedian(int *seq,double *pos,double *bounds,int *slen,double* wsize,int *rres,int *mres,int*wres)
{
	double step,cp;
	int start,stop,i;
	step = (bounds[1]-bounds[0])/(*rres);
	start = 0;
	stop  = 0;
	cp = bounds[0]+step/2;
	for (i=0; i<(*rres); i++)
	{
		while((pos[start] < cp-*wsize/2) && (start < *slen)) 
			start++;
		while((pos[stop] < cp+*wsize/2) && (stop < *slen)) 
			stop++;
		mres[i] = smedian(seq+start,stop-start);
		wres[i] = stop-start;
		cp += step;
	}
}
#endif

static double idx_pos(int idx,double *s,int len,double *bounds)
{
	return NDIV(idx,len)*(bounds[1]-bounds[0])+s[NMOD(idx,len)] ;
}

void pos_pmedian_c(int *seq,double *pos,double *bounds,int *slen,double* wsize,int *rres, double *pvalsr,double *pvalsa)
{
	double step,cp;
	int start,stop,i;
	Genltab(*slen);
	step = (bounds[1]-bounds[0])/(*rres);
	start = -(*slen);
	stop  = 0;
	cp = bounds[0]+step/2;
	for (i=0; i<(*rres); i++)
	{
		while((idx_pos(start,pos,*slen,bounds) < cp-*wsize/2) && (start < *slen)) 
			start++;
		while((idx_pos(stop,pos,*slen,bounds) < cp+*wsize/2) && (stop < 2**slen)) 
			stop++;
		CumPvalTB(*slen, stop-start,smedian2(seq,start,stop-start,*slen), pvalsr+i,pvalsa+i);
		cp += step;
	}
	free(ltab);
}
