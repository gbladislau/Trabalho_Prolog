%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Trabalho 1 Paradigmas de Programação -2023/2 %
% Nome: Gabriel Braga Ladislau - 2021101344    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%% Objetivo: Realizar inferências sobre músicas a
%% partir de dados disponíveis na Dbpedia (dbo:Song).
%
%% Faz a busca pelos dados sobre Songs na dbpedia
:- data_source(
   dbpedia_song,
   sparql(" 
          
           PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
		   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
		   PREFIX dbo: <http://dbpedia.org/ontology/>
           PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           PREFIX dboW: <http://dbpedia.org/ontology/Work/>
           select distinct ?titulo ?nomeArtista ?nomeAlbum 
           				   ?nomeGenero ?duracaoSeg ?dataDeLancamento 
           where {
            ?sujeito a dbo:Song ;
                      rdfs:label ?titulo;
           			  dbo:artist ?artist;
           			  dbo:releaseDate ?data;
           			  dbo:runtime ?runtime.
           
           	?artist foaf:name ?nomeArtista.
           
           OPTIONAL {?sujeito dbo:album ?album.
      				 ?album	foaf:name ?nomeAlbum.
                     ?sujeito dbo:genre ?genero.
      				 ?genero foaf:name ?nomeGenero .}
           
           
           BIND(xsd:date(?data) as ?dataDeLancamento)
           BIND(xsd:double(?runtime) as ?duracaoSeg)
           FILTER (lang(?titulo) = 'en')
           FILTER (lang(?nomeArtista) = 'en')
          }",
   [ endpoint('https://dbpedia.org/sparql')])  ).

% USADO PARA Criar OS FATOS 
musicas(Nome,Artista,Album,Genero,Duracao,DataDeLanc) :- dbpedia_song{titulo:Nome,
                                                                      nomeArtista:Artista,
                                                    				  nomeAlbum:Album,
                                                                      nomeGenero:Genero,
                                                    				  duracaoSeg:Duracao,        
                                                                      dataDeLancamento:DataDeLanc}.

% Abaixo temos seções para cada variavel da query utilizada, com diversas consultas,
% além de uma a mais com contadores + artistas, que eu não consegui organizar por conta da ordem!

% -------------------------------Artista---------------------------------------------- %
% Verifica se artista possui uma musica, deve ser unificada na consulta
% para ter o efeito desejado
artistaTemMusica(Artista) :- musicas(_,Artista,_,_,_,_),!.

% Unifica musica e artistas
musicaEArtista(Musica,Artista):- distinct([Musica],musicas(Musica,Artista,_,_,_,_)).

% Conta a quantidade de musicas dos artistas.
qntdDeMusicasDeArtista(Artista,Count) :- 
    distinct([Artista], musicas(_,Artista,_,_,_,_)),
    aggregate_all(count,(distinct([X],(musicas(X,Artista,_,_,_,_)))),Count).

% -------------------------------Duracao---------------------------------------------- %

% Verifica se a duração da musica é maior que um valor, Duracao deve estar unificada na consulta
musicasDeDuracaoMaiorQue(Musica,Duracao,D) :- 
    (   integer(Duracao) ->  distinct([Musica], musicas(Musica,_,_,_,D,_)), dif(D,'$null$'),
        D>Duracao; write("Digite uma Duração (Duracao deve ser unificada)")).

% Calcula a duração média de todas as musicas retornadas pela query (Assumo que a musica deve ter mais de 30s)
mediaDuracao(Media) :- 
    findall(Duracao, 
            (musicasDeDuracaoMaiorQue(_Musica,0,Duracao), dif(Duracao,'$null$'),Duracao > 30), Duracoes), 
    sum_list(Duracoes,Sum), proper_length(Duracoes,Len),
   	Media is Sum/Len.

% Inferindo tamanho a partir da média geral e da duração de certa musica
% Infere se uma musica é Grande (ou seja se sua duração é maior que a média de durações)
isMusicaGrande(Musica, Grande, D, M) :- 
    musicas(Musica,_,_,_,D,_), dif(D,'$null$'), mediaDuracao(M),
    (D > M -> Grande = true; Grande = false). 

% ------------------------------Data-------------------------------------------- %

% Procura musicas de acordo com dia mes e ano
musicasDessaData(Musica,Dia,Mes,Ano) :- musicas(Musica,_,_,_,_,Release),
    dif(Release,'$null$'), date_time_value(date,DT,Release), date_time_value('year', DT,Ano), 
    date_time_value('month', DT,Mes), date_time_value('day', DT,Dia).

% Unifica musicas e anos
musicasAno(Musica,Ano) :- musicas(Musica,_,_,_,_,date(Ano,_,_)).

% Procura a maior musica de certo ano, 
% para ter o efeito desejavel é melhor unificar o Ano antes,
% caso contrário a maior musica de todas sera retornada
maiorMusicaDeAno(Musica,Ano,Duracao) :- 
    order_by([desc(Duracao),desc(Ano)], 
             distinct([Musica,Duracao,Ano],
                      musicas(Musica,_,_,_,Duracao,date(Ano,_,_)))).

% ----------------------------------Genero----------------------------------------- %

% Conta quantas musicas cada genero possui
% OBS.: Aqui se tirar o dif vai mostrar a quantidade de dados sem genero
qntdDeMusicasDeGenero(Genero,Count) :-
    distinct([Genero], musicas(_,_,_,Genero,_,_)), dif(Genero, '$null$'),
    aggregate_all(count,(distinct([X],(musicas(X,_,_,Genero,_,_)))),Count).
%Sem dif
qntdDeMusicasDeGeneroDif(Genero,Count) :-
    distinct([Genero], musicas(_,_,_,Genero,_,_)),
    aggregate_all(count,(distinct([X],(musicas(X,_,_,Genero,_,_)))),Count).

% calcula a media de musicas por genero
mediaMusicasDosGen(Media) :-
    findall(Qntd, qntdDeMusicasDeGenero(_A,Qntd), Lista),  
    sum_list(Lista,Sum), proper_length(Lista,Len), Media is (Sum/Len).

% Infere-se que o genero é popular se sua quantidades de musicas totais
%  for maior que a média geral de todos os generos 
%  assumption bem naive mas é oque eu achei interessante
%  com os dados disponíveis
isGeneroPopular(Genero, Popular) :- 
	qntdDeMusicasDeGenero(Genero,Count), mediaMusicasDosGen(Media),
    (Count > Media -> Popular = true; Popular = false).

% ------------------------------------ Geral + Artista -------------------------------%
% Calcula a quantidade total de musicas da query
qntdDeMusicasTotais(C) :- aggregate_all(count,(distinct([X],(musicas(X,_,_,_,_,_)))),C).
                                                    
% Calcula a média de musicas totais dos Artistas presentes na query
mediaDeMusicasTotais(Media) :-
    findall(Qntd, qntdDeMusicasDeArtista(_A,Qntd), Lista),  
    sum_list(Lista,Sum), proper_length(Lista,Len), Media is (Sum/Len).

% Calcula a porcentagem de contribuicao dos artistas no somatorio total de musicas,
% ou seja,( total de musicas de  artista / total geral de musicas da query)
porcentagemDeContribuicaoDeArtistas(Porcentagem,Artista,Contador) :- 
	qntdDeMusicasDeArtista(Artista,Count), qntdDeMusicasTotais(T),
    Porcentagem is (Count/T), Contador = Count.
    
% Aqui fazemos a inferencia de artistas que produzem muito, sendo eles aqueles cuja soma
% do total de musicas é maior que a média do total de todos juntos.
artistaQueProduzMuito(Artista,ProduzMuito,Count,Media) :- 
    qntdDeMusicasDeArtista(Artista,Count), mediaDeMusicasTotais(Media),
    (Count > Media -> ProduzMuito = true; ProduzMuito = false).


% Unifica uma lista das musicas ordenadas pela suas duracoes
% ATENÇÂO: Ao rodar essa, vai encher o terminal com muita coisa :) !
ordenaMusicasPorTamanho(Musicas) :-
    findall([Duracao,Musica],order_by([desc(Duracao)],
                             (distinct([Musica],musicas(Musica,_,_,_,Duracao,_)),
                             dif(Duracao,""))),Musicas).


%------------------------------ Album -------------------------------------------%
% Retorna a quantidade de musica por album,
% Obs: se retirar a clausula do  dif aparece as musicas sem album 
qntdDeMusicasPorAlbum(Album,Count) :- 
    distinct([Album], musicas(_,_,Album,_,_,_)),dif(Album,'$null$'),
    aggregate_all(count,(distinct([X],(musicas(X,_,Album,_,_,_)))),Count).
% Sem Dif
qntdDeMusicasPorAlbumDif(Album,Count) :- 
    distinct([Album], musicas(_,_,Album,_,_,_)),
    aggregate_all(count,(distinct([X],(musicas(X,_,Album,_,_,_)))),Count).

% Media de musicas totais por album
mediaDeMusicasPorAlbum(Media):- 
    findall(Qntd, qntdDeMusicasPorAlbum(_A,Qntd), Lista),  
    sum_list(Lista,Sum), proper_length(Lista,Len), Media is (Sum/Len).

% Infere-se que um album é grande usando a mesma ideia das anteriores (qntd musicas > media).
% Aqui a média é bem pequena então os resultados são estranhos 
%(como quase tudo é 1 e a média é maior q 1 então fica quase todos falsos)
isAlbumGrande(Album,EhGrande,Qntd,Media) :- 
    qntdDeMusicasPorAlbum(Album,Qntd),
    mediaDeMusicasPorAlbum(Media),
	(Qntd > Media -> EhGrande = true; EhGrande  = false).

% ------------------------------ DECADA -----------------------------------%
% Infere-se aqui o conceito de década (se uma musica foi lançada em 2001 ela é da década de 2000)
% Unifica Musicas com suas respectivas Decadas
retornaDecadaDaMusica(Musica, Decada) :- 
    musicas(Musica,_,_,_,_, date(Ano,_Mes,_Dia)),
    dif(Ano,""), Decada is ((Ano//10)*10) .

% Unifica Musicas com suas respectivas Decadas e Duracoes
retornaMusicaDecadaDuracao(Musica,Decada,Duracao) :-  
    musicas(Musica,_,_,_,Duracao, date(Ano,_Mes,_Dia)),
    dif(Ano,""), Decada is ((Ano//10)*10) .

% Pega a maior musica de uma década, se não for unificado uma década pegara a maior de todas
maiorMusicaDaDecada(Musica,Duracao, Decada) :-
    findall([Duracao,Musica,Decada],order_by([desc(Duracao)],
                             (distinct([Musica],musicas(Musica,_,_,_,Duracao,_)),dif(Duracao,""),
                              retornaMusicaDecadaDuracao(Musica,Decada,Duracao))),
            Musicas),[H|_] = Musicas, [Duracao, Musica, Decada] = H.

% Verifica se uma musica pertence a certa década, neste caso decada deve ser unificada na consulta
ehMusicaDaDecada(Nome,Decada) :- integer(Decada), retornaDecadaDaMusica(Nome, D), D==Decada.

% EOF %