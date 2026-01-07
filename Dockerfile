FROM haskell:9.12.2-slim-bookworm AS builder
RUN echo cabal update date: 2026-01-05
RUN cabal update --verbose=2

WORKDIR /hs-paths2bins
COPY --link ./hs-paths2bins.cabal ./
RUN cabal update --verbose=2
RUN cabal build --only-dependencies
COPY --link ./app/ ./app/
COPY --link ./src/ ./src/
RUN cabal build
RUN cp $( cabal list-bin hs-paths2bins | fgrep --max-count=1 hs-paths2bins ) /usr/local/bin/
RUN which hs-paths2bins
RUN find . -type f | hs-paths2bins | while read line; do test -x "${line}" || exit 1; done

FROM debian:bookworm-slim
COPY --link --from=builder /usr/local/bin/hs-paths2bins /usr/local/bin/

CMD ["/usr/local/bin/hs-paths2bins"]
