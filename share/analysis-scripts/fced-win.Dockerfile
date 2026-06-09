FROM tobix/pywine:3.10
RUN mkdir /fced
COPY *.py /fced/
RUN mkdir /fced/dist-prepare
COPY fced-dist-prepare /fced/dist-prepare/
COPY fced-test /fced/fced-test/
COPY libc_metrics.json /fced/
WORKDIR /fced
RUN wine pyinstaller estimate_difficulty.py \
	  -F \
	  -n fc-estimate-difficulty.exe \
	  --distpath . \
	  --noconfirm \
	  --add-data "libc_metrics.json;share" \
	  --add-binary "dist-prepare/scc.exe;." \
	  --add-binary "dist-prepare/astyle.exe;."
RUN wine fc-estimate-difficulty.exe fced-test | \
    grep -A9 "Overall difficulty score" | \
    grep -v 0 | \
    grep -q ': '
