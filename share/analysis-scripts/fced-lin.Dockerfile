FROM alpine:3.15
COPY fc-estimate-difficulty /
RUN mkdir /fced-test
COPY fced-test fced-test/
RUN ./fc-estimate-difficulty fced-test | \
    grep -A9 "Overall difficulty score" | \
    grep -v 0 | \
    grep -q ': '
