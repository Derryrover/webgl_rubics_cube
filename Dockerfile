FROM node:7.2.0

RUN npm install -g elm@0.18.0
RUN npm install -g elm-test@0.18.0

#  probably not needed instead use CMD ["/bin/bash"] later
# RUN /bin/bash

# probably do not need next lines, should instead manually start reactor
# RUN elm reactor
# EXPOSE 8000 80

# make sure container keeps running
CMD ["/bin/bash"]

# probably do not need entrypoint
# ENTRYPOINT ["elm"]