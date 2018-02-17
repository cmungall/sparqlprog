FROM swipl:7.5.12

# Run the image as a non-root user
RUN useradd -m -s /bin/sh myuser
USER myuser
WORKDIR /home/myuser

ADD . $HOME

ENV PORT 9083
EXPOSE 9083

## RUN swipl -g "getenv('HOME',Home),atom_concat('file://',Home,Path),Opts=[interactive(false)],pack_install(Path,Opts),halt"
RUN swipl -g "Opts=[interactive(false)],pack_install(dcgutils,Opts),halt"

CMD swipl -p library=prolog -g "[bin/sprog_service]" -g 'server(9083),sleep(9999)'
