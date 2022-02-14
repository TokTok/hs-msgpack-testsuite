FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-msgpack-binary"]
COPY --chown=builder:users . /work/hs-msgpack-binary
RUN ["stack", "install", "msgpack-binary"]

FROM scratch
COPY --from=build /home/builder/.local/ /
