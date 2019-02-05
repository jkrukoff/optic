FROM erlang:21.2

# Install build and test dependencies.
# python3-pip: for installing python build dependencies.
RUN set -xe && apt-get update && apt-get install -y python3-pip

ENV REBAR="rebar3"
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

COPY ./requirements.txt ./
RUN pip3 install -r requirements.txt

COPY . ./
RUN make compile

ENTRYPOINT ["make"]
