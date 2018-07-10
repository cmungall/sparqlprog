FROM swipl:7.5.12

# Run the image as a non-root user
RUN useradd -m -s /bin/sh myuser
USER myuser
WORKDIR /home/myuser

ADD . $HOME

EXPOSE ${PORT}

## RUN swipl -g "getenv('HOME',Home),atom_concat('file://',Home,Path),Opts=[interactive(false)],pack_install(Path,Opts),halt"
RUN swipl -g "Opts=[interactive(false)],pack_install(dcgutils,Opts),pack_install(regex,Opts),halt"

CMD swipl -p library=prolog -g "[bin/sprog_service]" -g 'server,T is 10**10,sleep(T)'
