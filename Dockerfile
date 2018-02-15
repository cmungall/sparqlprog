FROM swipl:7.5.12

# Run the image as a non-root user
RUN useradd -m -s /bin/sh myuser
USER myuser
WORKDIR /home/myuser

ADD . $HOME

ENV PORT 9083
EXPOSE 9083

swipl -g "pack_install(sparqlprog)"

CMD ./bin/sprog-service --port=$PORT
