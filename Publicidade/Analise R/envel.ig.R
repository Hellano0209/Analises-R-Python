envel.ig <- function(modelo=fit.model,iden=0,nome=seq(along = model.matrix(modelo)[,1]),sim=100,conf=.90,res="D",link="1/mu^2",quad=T,maxit=20) {

#
# Descri��o e detalhes:
# A sa�da ser� o gr�fico de probabilidade normal com envelopes simulados para um ajuste da distribui��o normal inversa.
# � necess�rio que as fun��es para gerar normais inversas e calcular a sua fun��o de distribui��o acumulada estejam
# dispon�veis no R/S-Plus (respectivamente em rig e pig, essas fun��es est�o dentro do arquivo invgauss).
#
# A op��o res="C" faz o gr�fico de probabilidade meio-normal com envelopes simulados utilizando a dist�ncia de Cook,
# possibilitando a detec��o de pontos simultaneamente aberrantes e/ou influentes.
#
# O S-Plus s� permite ajustar a normal inversa com liga��o can�nica (1/mu^2). O R permite tamb�m com liga��es
# inverse e log. No entanto, o R tem um bug, pois armazena sempre no objeto ajustado que utilizou a liga��o can�nica.
# Assim, se utilizar uma liga��o que n�o seja a can�nica no R, deve-se informar para a fun��o atrav�s da op��o link.
#
# No R, quando um dos ajustes de cada simula��o n�o converge, a fun��o � interrompida. O S-Plus n�o faz isso, ent�o
# a fun��o simplesmente descarta os ajustes que n�o convergiram.
#
# Aten��o: a fun��o n�o funcionar� corretamente se o ajuste possuir offsets! Neste caso � preciso adapt�-la como foi
# feito na fun��o envel.pois
#
# Os dados devem estar dispon�veis pelo comando attach( ).
#
# Argumentos obrigat�rios:
# modelo: deve-se informar o objeto onde est� o ajuste do modelo, caso n�o seja informado, a fun��o procurar�
# 	  o ajuste no objeto fit.model;
# 
# Argumentos opcionais:
# iden: caso deseje, informe o n�mero de observa��es que ir� querer destacar. O padr�o � n�o destacar ningu�m (iden=0).
#	Qualquer valor que n�o seja um inteiro positivo (por ex., negativo ou decimal) far� com que a fun��o pergunte
#	o n�mero de pontos ap�s a execu��o;
# nome: esse argumento s� � utilizado caso seja destacado algum ponto no gr�fico. Caso n�o seja informado nada, os pontos
#	identificados ser�o os n�meros da ordem em que est�o no banco de dados (os �ndices). Caso se queira, pode-se
#	informar um vetor de nomes ou de identifica��es alternativas. Obrigatoriamente esse vetor deve ter o mesmo
#	comprimento do banco de dados;
# sim: n�mero de simula��es para gerar a banda de confian�a. Atkinson sugere um m�nimo de 20 simula��es.
#      O padr�o � de 100;
# conf: n�vel de confian�a do envelope. O padr�o � de 90%;
# res: permite-se a escolha dos res�duos. As op��es dos res�duos s�o: "Q" quantil (ver Dunn e Smyth, 1996), "D" componente
#      do desvio, "P" Pearson padronizado, "A" Anscombe, "W" Williams e "C" dist�ncia de Cook. A op��o padr�o � a "D";
# link: padr�o "1/mu^2", que � a liga��o can�nica (1/mu^2). Se utilizar a liga��o log, informe "log" e se utilizar
#	a liga��o inversa, informe "inverse";
# quad: o padr�o (quad=T, True) faz um gr�fico quadrado, enquanto quad=F (False) faz um gr�fico utilizando a �rea m�xima
#       dispon�vel;
# maxit: essa op��o � utilizada nos ajustes de cada simula��o e indica o m�ximo de itera��es permitidas nos ajustes.
#	 O padr�o � maxit=20.
#
# Autor: Frederico Zanqueta Poleto <fred@poleto.com>, arquivo dispon�vel em http://www.poleto.com
#
# Refer�ncias:
# DUNN, K. P., and SMYTH, G. K. (1996). Randomized quantile residuals. J. Comput. Graph. Statist. 5, 1-10
#    [http://www.statsci.org/smyth/pubs/residual.html e http://www.statsci.org/smyth/pubs/residual.ps]
# MCCULLAGH, P. e NELDER, J. A. (1989). Generalized Linear Models. 2� ed. Chapman and Hall, London.
# PAULA, G. A. (2003). Modelos de Regress�o com apoio computacional. IME-USP, S�o Paulo. [N�o publicado,
#    dispon�vel em http://www.ime.usp.br/~giapaula/Book.pdf]
#
# Exemplos:
# envel.ig(ajuste,sim=1000,conf=.95,maxit=50)
# envel.ig(ajuste,res="C")
#

if(class(modelo)[1] != "glm") {
	stop(paste("\nA classe do objeto deveria ser glm e nao ",class(modelo),"!!!\n"))
}
if(modelo$family[[1]] != "Inverse Gaussian" & modelo$family[[1]] != "inverse.gaussian") {
	stop(paste("\nA familia do objeto deveria ser da normal inversa !!!\n"))
}
if(link!="1/mu^2") {
	if(is.null(version$language) == T) {
		stop(paste("\nO S-Plus s� aceita a liga��o can�nica!!!\n"))
	} else {
		if(link!="inverse" & link!="log") {
			stop(paste("\nO R s� aceita as liga��es can�nica (1/mu^2), inversa (1/mu) e log!!!\n"))
		}
	}
}

alfa<-(1-conf)/2
X <- model.matrix(modelo)
n <- nrow(X)
p <- ncol(X)
w <- modelo$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)

#para evitar divis�o por 0 ao studentizar os residuos, mas tentando manter o valor exagerado da alavanca
h[round(h,15)==1]<-0.999999999999999

m<-predict(modelo,type="response")
y<-modelo$y
fi<-(n-p)/sum((resid(modelo,type="response")/(m*sqrt(y)))^2) #� igual a (n-p)/sum((y-m)^2/sqrt(m^2*y))

if(res=="Q") {
	tipo<-"Res�duo Quantil"
	r<-qnorm( pig(y,m,lambda=fi) )
} else {
	if(res=="D") {
		tipo<-"Res�duo Componente do Desvio"
		r<-resid(modelo,type="deviance")*sqrt(fi/(1-h))
	} else {
		if(res=="P") {
			tipo<-"Res�duo de Pearson Padronizado"
			r<-resid(modelo,type="pearson")*sqrt(fi/(1-h))
		} else {
			if(res=="A") {
				tipo<-"Res�duo de Anscombe"
				r<-sqrt(fi)*( log(y) - log(m) )/sqrt(m)
			} else {
				if(res=="W") {
					tipo<-"Res�duo de Williams"
					r<-sign(y-m)*sqrt((1-h)*(( resid(modelo,type="deviance")*sqrt(fi/(1-h)) )^2)+(h*( resid(modelo,type="pearson")*sqrt(fi/(1-h)) )^2))
				} else {
					if(res=="C") {
						tipo<-"Dist�ncia de Cook"
						r<-(h/((1-h)*p))*((resid(modelo,type="pearson")/sqrt(1-h))^2)
					} else {
						stop(paste("\nVoce nao escolheu corretamente um dos residuos disponiveis!!!\n"))
					}
				}
			}
		}
	}
}

e <- matrix(0,n,sim)
e1 <- numeric(n)
e2 <- numeric(n)

if (is.null(version$language) == T) {
	#No S-Plus, a op��o start � para entrar com o preditor linear
	pm<-predict(modelo)
} else {
	#No R, a op��o start � para entrar com os coeficientes
	pm<-coef(modelo)
}
mu<-m
i<-1
tot<-0
while(i <= sim) {
	tot<-tot+1
	if(tot>(10*sim)) {
		stop(paste("\nA funcao descarta ajustes que nao convergiram. Ja foram quase 10 vezes o numero de simulacoes solicitadas!\n"))
	}
	resp <- rig(n,mu,fi)
	if (link == "1/mu^2") {
		fit <- glm(resp ~ X-1,family=inverse.gaussian,maxit=maxit,start=pm)
	} else {
		if (link == "inverse") {
			fit <- glm(resp ~ X-1,family=inverse.gaussian(link=inverse),maxit=maxit,start=pm)
		}
		if (link == "log") {
			fit <- glm(resp ~ X-1,family=inverse.gaussian(link=log),maxit=maxit,start=pm)
		}
	}
	if (is.na(deviance(fit))==F) {
		w <- fit$weights
		W <- diag(w)
		H <- solve(t(X)%*%W%*%X)
		H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
		h <- diag(H)
		h[round(h,15)==1]<-0.999999999999999
		m <- predict(fit,type="response")
		y <- fit$y
		phi <- (n-p)/sum((resid(modelo,type="response")/(m*sqrt(y)))^2)
		e[,i] <- 
		sort( if(res=="Q") {
			qnorm( pig(y,m,lambda=phi) )
		} else {
			if(res=="D") {
				resid(fit,type="deviance")*sqrt(phi/(1-h))
			} else {
				if(res=="P") {
					resid(fit,type="pearson")*sqrt(phi/(1-h))
				} else {
					if(res=="A") {
						sqrt(phi)*( log(y) - log(m) )/sqrt(m)
					} else {
						if(res=="W") {
							sign(y-m)*sqrt((1-h)*(( resid(fit,type="deviance")*sqrt(phi/(1-h)) )^2)+(h*( resid(fit,type="pearson")*sqrt(phi/(1-h)) )^2))
						} else {
							if(res=="C") {
								(h/((1-h)*p))*((resid(fit,type="pearson")/sqrt(1-h))^2)
							} else {
								stop(paste("\nVoce nao escolheu corretamente um dos residuos disponiveis!!!\n"))
							}
						}
					}
				}
			}
		})
		i<-i+1
	}
}

for(i in 1:n) {
	eo <- sort(e[i,])
	e1[i] <- quantile(eo,alfa)
	e2[i] <- quantile(eo,1-alfa)
}

med <- apply(e,1,median)

if(quad==T) {
	par(pty="s")
}
if(res=="C") {
	#Segundo McCullagh e Nelder (1989, p�g.407) e Paula (2003, p�g.57) deve-se usar qnorm((n+1:n+.5)/(2*n+1.125))
	#Segundo Neter et alli (1996, p�g.597) deve-se usar qnorm((n+1:n-.125)/(2*n+0.5))
	qq<-qnorm((n+1:n+.5)/(2*n+1.125))
	plot(qq,sort(r),xlab="Quantil Meio-Normal",ylab=tipo, ylim=range(r,e1,e2), pch=16)
} else {
	qq<-qnorm((1:n-.375)/(n+.25))
	plot(qq,sort(r),xlab="Quantil da Normal Padr�o",ylab=tipo, ylim=range(r,e1,e2), pch=16)
}
lines(qq,e1,lty=1)
lines(qq,e2,lty=1)
lines(qq,med,lty=2)
nome<-nome[order(r)]
r<-sort(r)
while ( (!is.numeric(iden)) || (round(iden,0) != iden) || (iden < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden<-as.numeric(out)
}
if(iden>0) {identify(qq,r,n=iden,labels=nome)}
if(quad==T) {
	par(pty="m")
}
cat("Banda de ",conf*100,"% de confianca, obtida por ",sim," simulacoes.\n")
if (is.null(version$language) == T) {
	if (tot > sim) {
		cat("Ignore os warnings que dizem \"iterations terminated prematurely because of singularities\", pois eles nao foram utilizados na construcao da banda de confianca.")
	}
} else {
	cat("Ligacao utilizada:",link,". Verifique se foi a mesma que utilizou no ajuste ou informe corretamente!")
}
cat("\n")
}