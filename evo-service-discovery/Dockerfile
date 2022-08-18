FROM adoptopenjdk/openjdk11-openj9:ubi-minimal-jre

# Set the current working directory inside the image
WORKDIR /app

COPY ./target/evo-service-discovery-1.0-SNAPSHOT.jar /app/evo-service-discovery-0.0.1.jar
ADD ./src/main/resources /app/config

EXPOSE 80

ENTRYPOINT ["java","-jar","evo-service-discovery-0.0.1.jar","--spring.profiles.active=${ENV:dev}"]
