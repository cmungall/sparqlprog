FROM swipl:8.0.3

# Run the image as a non-root user
RUN useradd -m -s /bin/sh myuser
USER myuser
WORKDIR /home/myuser

ADD . $HOME

USER root
RUN chown myuser tests tests/* && ls -alt tests
USER myuser

EXPOSE ${PORT}

## RUN swipl -g "getenv('HOME',Home),atom_concat('file://',Home,Path),Opts=[interactive(false)],pack_install(Path,Opts),halt"
RUN swipl -g "Opts=[interactive(false)],pack_install(dcgutils,Opts),pack_install(regex,Opts),pack_install(typedef,Opts),halt"
RUN swipl -p library=prolog -l tests/tests.pl -g run_tests,halt

CMD swipl -p library=prolog -g "[bin/sprog_service]" -g 'server,T is 10**10,sleep(T)'
