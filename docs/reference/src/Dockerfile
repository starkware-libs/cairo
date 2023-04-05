FROM python:3.9-alpine
RUN apk add --update gmp-dev build-base nodejs npm git zsh curl
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
RUN python -m pip install --upgrade pip
RUN pip install cairo-lang openzeppelin-cairo-contracts pytest pytest-asyncio starknet-devnet starknet.py
WORKDIR /app