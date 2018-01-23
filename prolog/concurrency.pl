/* Part of sparkle
	Copyright 2014-2015 Samer Abdallah (UCL)
	 
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(concurrency, [concurrent_or/1, concurrent_or/3]).

:- meta_predicate concurrent_or(-,:,+).
:- meta_predicate concurrent_or(:).

%% concurrent_or( +Goals:list(callable)) is nondet.
%
%  Simple interface to concurrent_or/3. Equivalent to
%  concurrent_or(Vars,Goals,[]) where Vars is a list of all the variables in
%  Goals.
concurrent_or(Goals) :-
   term_variables(Goals,Vars),
   concurrent_or(Vars,Goals,[]).

%% concurrent_or( -Vars, +Goals:list(callable), +Options:list(option)) is nondet.
%
%  Succeeds once for each solution of each goal in Goals, with Vars bound
%  to sharing variables in Goals. Goals are executed in parallel. Valid
%  options are
%     *  on_error(OnError:oneof([stop,continue]))
%        If OnError=stop, then an exception occuring in any goal stops all
%        goals and is propagated back to and then thrown from the main thread.
%        If OnError=continue, then an exception in a goal terminates only 
%        that thread, with a error message printed. The default is stop.
%     *  queue_factor(K:natural)
%        Solutions are communicated via a message queue of size K*length(Goals).
%        This limits the extent to which threads compute solutions that have not
%        yet been requested. The default is 1.
%  Any remaining options are passed to thread_create/3.

concurrent_or(Vars, M:List, Options) :-
   select_option(on_error(OnError),Options,Opts1,stop),
   select_option(queue_factor(K),Opts1,Opts2,1),
   length(List, JobCount),
   QueueSize is K*JobCount,
   message_queue_create(Done,[max_size(QueueSize)]),
   setup_call_cleanup(
      maplist(create_worker(M,Vars,Done,Opts2),List,Solvers),
      wait_for_one(JobCount, Done, Vars, OnError),
      (  debug(concurrency,'Sending kill signal to workers',[]),
         maplist(kill_thread,Solvers), drain(Done),
         debug(concurrency,'Waiting for workers to die.',[]),
         maplist(thread_join,Solvers,_),
         message_queue_destroy(Done)
      )
   ).

drain(Q) :- thread_get_message(Q,_,[timeout(0)]) -> drain(Q); true.
kill_thread(Id) :- catch(thread_signal(Id,throw(abort)),_,true).
create_worker(M,V,Q,O,H,Id) :- thread_create(worker(M:H,V,Q),Id,O).

wait_for_one(N, Q, X, OnError) :-
   succ(N1,N),
   thread_get_message(Q, Msg),
   (  Msg=success(_,Var) -> (X=Var; wait_for_one(N,Q,X,OnError))
   ;  Msg=failed(_)      -> wait_for_one(N1,Q,X,OnError)
   ;  Msg=error(_,E)     -> ( OnError=stop -> throw(error(E))
                            ; print_message(error,E),
                              wait_for_one(N1,Q,X,OnError)
                            )
   ).

worker(Goal,Var,Q) :-
   thread_self(Me),
   debug(concurrency,'Worker started on ~q.',[Goal]),
   (  catch( Goal,E, (thread_send_message(Q,error(Me,E)), throw(error))),
      thread_send_message(Q,success(Me,Var)), fail
   ;  thread_send_message(Q,failed(Me)),
      debug(concurrency,'Worker finished normally.',[])
   ).
