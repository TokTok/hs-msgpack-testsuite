FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-msgpack-testsuite"]
COPY --chown=builder:users . /work/hs-msgpack-testsuite
RUN ["stack", "install", "msgpack-testsuite"]

FROM scratch
COPY --from=build /home/builder/.local/ /
