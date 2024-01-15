% 107502 - Francisco Ferro Pereira, LEIC-T

:- set_prolog_flag(answer_write_options,[max_depth(0)]). 
:- ['../knowledge_base/dados.pl'], ['../knowledge_base/keywords.pl'].


/* 
eventosSemSalas(EventosSemSala) devolve uma lista, ordenada e sem elementos repetidos, 
de IDs de eventos sem sala. 																			*/
eventosSemSalas(EventosSemSala):-
	findall(ID,evento(ID,_,_,_,semSala),EventosSemSala).




/* 
eventosSemSalasDiaSemana(DiaSemana, EventosSemSala) recebe um dia da semana (DiaSemana), e devolve 
uma lista ordenada e sem elementos repetidos de IDs de eventos sem sala que decorrem em DiaSemana.
																										*/                                                                        
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala):-			
	
	eventosSemSalas(Y),	
	
	findall(ID,horario(ID,DiaDaSemana,_,_,_,_),Z),
	
	intersection(Y,Z,EventosSemSala).
			



/* 
append_if_not_vazia eh um predicado auxiliar de eventosSemSalasPeriodo, que recebe como argumentos um elemento (El) e
uma lista (Lista) e apenas adiciona El a Lista caso esta nao esteja vazia                                               */
append_if_not_vazia(El, Lista, Res) :-   
	length(Lista, Length), 
	Length > 0 -> append([El],Lista, Res); Res = [].
    



/* 
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) eh verdade se ListaPeriodos eh uma lista de periodos 
e EventoSemSala eh uma lista ordenada e sem elementos repetidos de IDs de eventos sem sala que decorrem nos 
periodos em ListaPeriodos.                       																	*/

eventosSemSalasPeriodo([],[]).
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) :-
    
	% Adiciona p1 ou p2 a Sem1 se estiverem na ListaPeriodos 
	findall(P, (member(P, ListaPeriodos), (P = p1; P = p2)), Sem1),   
    
	% Adiciona p3 ou p4 a Sem2 se estiverem na ListaPeriodos 
	findall(P, (member(P, ListaPeriodos), (P = p3; P = p4)), Sem2),   
    
	% Adiciona p1_2 se estiver la p1 ou p2
	append_if_not_vazia(p1_2, Sem1, ListaPeriodosAtualizada1),  
	
	% Adiciona p3_4 se estiver la p3 ou p4
	append_if_not_vazia(p3_4, Sem2, ListaPeriodosAtualizada2),  
	
	% Junta as listas 
	append(ListaPeriodosAtualizada1, ListaPeriodosAtualizada2, ListaPeriodosAtualizada3), 
    
	eventosSemSalas(ListaEventosSemSala),

	findall(ID, (member(ID,ListaEventosSemSala), horario(ID,_,_,_,_,Periodo), 
	member(Periodo, ListaPeriodosAtualizada3)), EventosSemSala).




/* 
ocorreNoPeriodo(ID,Periodo) eh um predicado auxiliar de OrganizaEventos. 
Eh verdade se o ID corresponde a um evento no Periodo. (tem em conta as semestrais).
																					*/
ocorreNoPeriodo(ID, Periodo) :- 
	horario(ID, _, _, _, _, P),
  
	(Periodo = p1 -> (P = p1 ; P = p1_2);
	Periodo = p2 -> (P = p2 ; P = p1_2);
	Periodo = p3 -> (P = p3 ; P = p3_4);
	Periodo = p4 -> (P = p4 ; P = p3_4)).
 


/* 
organizaEventos_Not_Sorted eh um predicado auxiliar de organizaEventos onde ocorre a recursao, 
mas nao da sort e nao remove os duplicados. 															*/

organizaEventos_Not_Sorted([],_,[]).			  

organizaEventos_Not_Sorted([Evento|ListaEventos], Periodo, EventosNoPeriodo) :-

	% Verifica se a cabeca ocorre no Periodo
	ocorreNoPeriodo(Evento,Periodo),

	% Adiciona a cabeca a EventosNoPeriodo 
	EventosNoPeriodo = [Evento|EventosNoPeriodo1],								   
		
	organizaEventos(ListaEventos, Periodo, EventosNoPeriodo1).					   
		
% Cabeca da lista nao pertence ao Periodo
organizaEventos_Not_Sorted([_|ListaEventos], Periodo, EventosNoPeriodo) :-	       
		
	organizaEventos(ListaEventos, Periodo, EventosNoPeriodo).				      
	


/* 
organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) eh verdade se EventosNoPeriodo eh uma lista ordenada 
e sem elementos repetidos de IDs dos eventos de ListaEventos que ocorrem no periodo Periodo.                                    
																							                  */

% PREDICADO PRINCIPAL

organizaEventos(ListaEventos, Periodo, EventosNoPeriodo):-
		
		% Chamada do predicado auxiliar
		organizaEventos_Not_Sorted(ListaEventos,Periodo,Res),
		
		% Remove IDs duplicados		
		list_to_set(Res,ListaEventosSemRepetidos),	

		sort(ListaEventosSemRepetidos,EventosNoPeriodo).			  	
	
	


/*
eventosMenoresQue(Duracao, ListaEventosMenoresQue) eh verdade se ListaEventoMenoresQue eh uma lista ordenada 
e sem elementos repetidos de IDs de eventos que tem duracao menor ou igual a Duracao.
																											 */
eventosMenoresQue(Duracao, ListaEventosMenoresQue):-
	findall(ID,(horario(ID,_,_,_,X,_), X =< Duracao),ListaEventosMenoresQue).




/* 
eventosMenoresQueBool(ID, Duracao) eh verdade se o evento identificado por ID tiver duracao menor ou igual a Duracao.      
																													*/
eventosMenoresQueBool(ID,Duracao) :-
	
	findall(DuracaoEvento,horario(ID,_,_,_,DuracaoEvento,_),X),
	
	nth1(1,X,Y), 
	
	Y =< Duracao.




/* 
procuraDisciplinas(Curso, ListaDisciplinas) eh verdade se ListaDisciplinas eh a lista ordenada alfabetifamente do 
nome das disciplinas do curso Curso.  																				*/

procuraDisciplinas(Curso,ListaDisciplinas):-

	% Vai buscar IDs dos eventos do curso
	findall(ID,turno(ID,Curso,_,_),EventosDoCurso),                   
		
    % Remove IDs duplicados 
	list_to_set(EventosDoCurso,EventosDoCurso1),                      
	
	% Vai buscar os nomes das disciplinas
	findall(NomeDisciplina,(evento(ID,NomeDisciplina,_,_,_),member(ID,EventosDoCurso1)),DisciplinasNotSorted),

	sort(DisciplinasNotSorted,ListaDisciplinas).




/* 
organizaDisciplinas(ListaDisciplinas, Curso, Semestres) eh verdade se Semestres eh uma lista com duas listas. A primeira
lista contem as disciplinas de ListaDisciplinas que decorrem no primeiro semestre e a segunda contem as do segundo. 
Ambas estao alfabeticamente ordenadas e sem elementos repetidos. O predicado falha se nao existir no Curso uma disciplina 
de ListaDisciplinas.                                                                                                      */

organizaDisciplinas(ListaDisciplinas, Curso, Semestres) :-

	% Se ListaDisciplinas eh vazia, entao falha
	(ListaDisciplinas == [] -> fail; true),

	% Verifica se ListaDisciplinas eh um subset de CadeirasCurso
	procuraDisciplinas(Curso, CadeirasCurso),
	subset(ListaDisciplinas, CadeirasCurso),
	
% Inicializa-se duas listas vazias, correspondentes a Sem1 e Sem2.
organizaDisciplinas(ListaDisciplinas, Curso, [], [], Semestres).

organizaDisciplinas([Disciplina|T], Curso, Sem1, Sem2, Semestres) :-
	    
    (evento(ID, Disciplina, _, _, _), 
	turno(ID, Curso, _, _), 
	horario(ID, _, _, _, _, Periodo),
	
    % Se Periodo for p1, p2 ou p1_2, adicionamos Disciplina a Sem1 
    member(Periodo, [p1, p2, p1_2]) -> organizaDisciplinas(T, Curso, [Disciplina|Sem1], Sem2, Semestres); 
    
    % Caso contrario, pertence a Sem2, pelo que adicionamos Disciplina a Sem2 
    organizaDisciplinas(T, Curso, Sem1, [Disciplina|Sem2], Semestres)).

% Da sort no fim
organizaDisciplinas([], _, Semestre1, Semestre2, [S1, S2]) :-
	sort(Semestre1,S1),
	sort(Semestre2,S2).





/* 
horasCurso(Periodo, Curso, Ano, TotalHoras) eh verdade se TotalHoras for o numero de horas total dos eventos
associados ao curso Curso, no ano Ano e periodo Periodo.
                  																								*/
horasCurso(Periodo, Curso, Ano, TotalHoras) :-

	% Configura os periodos pelos quais queremos pesquisar
	(Periodo = p1 -> (ListaPesquisa = [p1,p1_2]);
	Periodo = p2 -> (ListaPesquisa = [p2,p1_2]);
	Periodo = p3 -> (ListaPesquisa = [p3,p3_4]);
	Periodo = p4 -> (ListaPesquisa = [p4,p3_4])),
		
	% Vai buscar IDs dos turnos do curso.
	findall(ID,turno(ID,Curso,Ano,_),L1),

	list_to_set(L1,L2),

	% Vai buscar IDs do curso que estao no Periodo. 
	findall(ID,(horario(ID,_,_,_,_,P), member(P,ListaPesquisa),member(ID,L2)),L3),
			
	% Vai buscar as duracoes dos eventos no Periodo.
	findall(Duracao,(horario(ID,_,_,_,Duracao,_),member(ID,L3)),L4),

	sum_list(L4,TotalHoras).




/* 
evolucaoHorasCurso(Curso, Evolucao) eh verdade se Evolucao for uma lista de tuplos na forma (Ano, Periodo, NumHoras), 
em que NumHoras eh o total de hora associadas ao curso Curso, no ano Ano e periodo Periodo.   
																														*/
evolucaoHorasCurso(Curso, Evolucao) :-
	
	findall((Ano, Periodo, NumHoras), (member(Ano,[1,2,3]), member(Periodo, [p1, p2, p3, p4]), 
			horasCurso(Periodo, Curso, Ano, NumHoras)), Evolucao).
	
	
	
/* 
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) eh verdade se Horas for o numero de
horas sobrepostas entre o evento que tem inicio em HoraInicioEvento e fim em HoraFimEvento,e o slot que tem inicio
em HoraInicioDada e fim em HoraFimDada. Se nao existir sobreposicao, o predicado falha.                            */

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):-
	
	max_list([HoraInicioDada,HoraInicioEvento], InicioOverlap),
		
	min_list([HoraFimDada,HoraFimEvento], FimOverlap),
		 
	(Horas is FimOverlap - InicioOverlap),
		   
	(Horas =< 0 -> fail; true).



/* 
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) eh verdade se SomaHoras for o numero de horas
ocupadas nas salas do tipo TipoSala, no intervalo de tempo definido entre HoraInicio e HoraFim, no dia da semana DiaSemana, e
no periodo Periodo.                                                                                               
																												             */
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-

		% Configura os periodos pelos quais queremos pesquisar
		(Periodo = p1 -> ( ListaPesquisa = [p1,p1_2] );
		Periodo = p2 -> ( ListaPesquisa = [p2,p1_2] );
		Periodo = p3 -> ( ListaPesquisa = [p3,p3_4] );
		Periodo = p4 -> ( ListaPesquisa = [p4,p3_4] )),
	
		
	salas(TipoSala,SalasDoTipo),
		
	% Vai buscar os IDs dos eventos que decorrem nas salas do TipoSala, no DiaSemana e no Periodo
	findall(ID, ( member(Sala,SalasDoTipo), evento(ID,_,_,_,Sala), member(P, ListaPesquisa), 
		    horario(ID, DiaSemana, _, _, _, P)), ListaEventosSalasDoTipo),
	
	
	% Vai buscar o numero de horas em que os eventos se sobrepoem no intervalo, de todos os horarios cujo ID pertence a ListaEventosSalasDoTipo
	findall( HorasOcupadas, ( member(ID, ListaEventosSalasDoTipo), horario(ID, _, HoraInicioEvento, HoraFimEvento, _, _), 
	        ocupaSlot( HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, HorasOcupadas)), L),
	
	sum_list(L, SomaHoras).
		



/*
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) eh verdade se Max for numero de horas possiveis de ser ocupadas por salas
do tipo TipoSala, no intervalo de tempo definido entre HoraInicio e HoraFim. Max eh o intervalo de tempo dado, multiplicado
pelo numero de salas em jogo do tipo TipoSala.                               												*/

ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-
	
	salas(TipoSala, Salas),

	length(Salas, L),

	Max is ((HoraFim - HoraInicio) * L). 



/*
percentagem(SomaHoras, Max, Percentagem) eh verdade se Percentagem for a divisao de SomaHoras por Max, multiplicada por 100   
																															*/
percentagem(SomaHoras, Max, Percentagem) :-
	Percentagem is ((SomaHoras / Max) * 100).

		

/*
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) eh verdade se Resultados for uma lista ordenada de tuplos de
do tipo casosCriticos(DiaSemana, TipoSala, Percentagem), casos esses que decorrem entre HoraInicio e HoraFim, em salas
do TipoSala, cuja percentagem de ocupacao eh superior a um valor critico, o Threshold.									    */

ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :-

			ListaPeriodos = [p1, p2, p1_2, p3 ,p4, p3_4],
			Dias = [segunda-feira, terca-feira, quarta-feira, quinta-feira, sexta-feira],
			
			% Todos os tipos de sala
			findall( TipoSala, salas( TipoSala, _), Tipos),
			
			
			% Vai buscar os casosCriticos
			findall( casosCriticos(DiaSemana, TipoSala, PercentagemArredondada), 
			
			(member(Periodo, ListaPeriodos), member(DiaSemana, Dias), member(TipoSala, Tipos), 
			
			numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras), 
			
			ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max), 
			
			percentagem(SomaHoras, Max, Percentagem), Percentagem > Threshold, ceiling( Percentagem, PercentagemArredondada)),L),
			
			sort(L,Resultados).
				
	
			
			
/* cab1 eh verdade se NomePessoa esta na cabeceira1 na lista OcupacaoMesa */
cab1(NomePessoa,OcupacaoMesa) :-	
	OcupacaoMesa = [[_,_,_],[NomePessoa,_],[_,_,_]].


/* cab2 eh verdade se NomePessoa esta na cabeceira2 na lista OcupacaoMesa */
cab2(NomePessoa,OcupacaoMesa):-
	OcupacaoMesa = [[_,_,_],[_,NomePessoa],[_,_,_]].


/* honra eh verdade se NomePessao1 esta numa das cabeceiras da mesa e NomePessoa2 na sua direita na lista OcupacaoMesa */
honra(NomePessoa1, NomePessoa2, OcupacaoMesa) :-		
		OcupacaoMesa = [[_,_,_],[NomePessoa1,_],[NomePessoa2,_,_]];
		OcupacaoMesa = [[_,_,NomePessoa2],[_,NomePessoa1],[_,_,_]].


/* lado eh verdade se NomePessoa1 e NomePessoa2 estao lado a lado na lista OcupacaoMesa  */
lado(NomePessoa1, NomePessoa2, OcupacaoMesa) :-
			
		OcupacaoMesa = [Lado1,_,Lado3],

		( nextto(NomePessoa1, NomePessoa2, Lado1)
		; nextto(NomePessoa2, NomePessoa1, Lado1)
		; nextto(NomePessoa1, NomePessoa2, Lado3)
		; nextto(NomePessoa2, NomePessoa1, Lado3)).


/* naoLado eh verdade se NomePessoa1 e NomePessoa2 nao estao lado a lado na lista OcupacaoMesa  */
naoLado(NomePessoa1,NomePessoa2, OcupacaoMesa) :-
	\+lado(NomePessoa1,NomePessoa2,OcupacaoMesa).


/* frente eh verdade se NomePessoa1 e NomePessoa2 estao frente a frente na lista OcupacaoMesa  */
frente(NomePessoa1,NomePessoa2, OcupacaoMesa) :-

	OcupacaoMesa = [[NomePessoa1,_,_],[_,_],[NomePessoa2,_,_]];
	OcupacaoMesa = [[NomePessoa2,_,_],[_,_],[NomePessoa1,_,_]];
	OcupacaoMesa = [[_,NomePessoa1,_],[_,_],[_,NomePessoa2,_]];
	OcupacaoMesa = [[_,NomePessoa2,_],[_,_],[_,NomePessoa1,_]];
	OcupacaoMesa = [[_,_,NomePessoa1],[_,_],[_,_,NomePessoa2]];
	OcupacaoMesa = [[_,_,NomePessoa2],[_,_],[_,_,NomePessoa1]].


/* naoFrente eh verdade se NomePessoa1 e NomePessoa2 nao estao frente a frente na lista OcupacaoMesa */
naoFrente(NomePessoa1, NomePessoa2, OcupacaoMesa) :-
	\+frente(NomePessoa1,NomePessoa2, OcupacaoMesa).

			

/* 
verificaRestricoes(ListaPessoa, ListaRestricoes, OcupacaoMesa) eh um predicado auxiliar de ocupacaMesa 
que verifica se a permutacao atual de ListaPessoas cumpre as restricoes presentes em ListaRestricoes.  */

verificaRestricoes(ListaPessoas, ListaRestricoes, OcupacaoMesa) :-
	
	% Itera por todas as restricoes em ListaRestricoes
	forall(member(Restricao, ListaRestricoes),
	
	% Vai buscar os Argumentos da restricao
	(Restricao =.. [_|Argumentos],
	
	% Verifica que Argumentos sao nomes em ListaPessoas
	subset(Argumentos,ListaPessoas),
	
	% Chama o predicado da restricao com OcupacaoMesa
	call(Restricao, OcupacaoMesa))).
	
	



/*
ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) eh verdade se ListaPessoas for a lista com os nomes das pessoas
a sentar na mesa, ListaRestricoes a lista de restricoes a verificar e ocupacaoMesa uma lista com 3 listas em que a primeira
contem as pessoas de um lado da mesa, a segunda, as pessoas nas cabeceiras e a terceira as pessoas do outro lado da mesa.
																															*/
ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) :-	
	
	% Permuta a ListaPessoas  
	permutation(ListaPessoas, Permutacao),

	% Retira cada uma das pessoas da lista Permutacao	
	nth1(1, Permutacao, P1),
    nth1(2, Permutacao, P2),
    nth1(3, Permutacao, P3),
    nth1(4, Permutacao, P4),
    nth1(5, Permutacao, P5),
    nth1(6, Permutacao, P6),
    nth1(7, Permutacao, P7),
    nth1(8, Permutacao, P8),
    
	% Estrutura a disposicao dos lugares
	OcupacaoMesa = [[P1,P2,P3],[P4,P5],[P6,P7,P8]],

	% Verifica as restricoes (caso falhe, avanca para permutacao de ListaPessoas seguinte)		
	verificaRestricoes(ListaPessoas, ListaRestricoes, OcupacaoMesa).

	