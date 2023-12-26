FROM mcr.microsoft.com/dotnet/runtime:8.0 AS base
USER $APP_UID
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
ARG BUILD_CONFIGURATION=Release
WORKDIR /src
COPY ["AdventOfCode2023.Day06/AdventOfCode2023.Day06.fsproj", "AdventOfCode2023.Day06/"]
RUN dotnet restore "AdventOfCode2023.Day06/AdventOfCode2023.Day06.fsproj"
COPY . .
WORKDIR "/src/AdventOfCode2023.Day06"
RUN dotnet build "AdventOfCode2023.Day06.fsproj" -c $BUILD_CONFIGURATION -o /app/build

FROM build AS publish
ARG BUILD_CONFIGURATION=Release
RUN dotnet publish "AdventOfCode2023.Day06.fsproj" -c $BUILD_CONFIGURATION -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "AdventOfCode2023.Day06.dll"]
