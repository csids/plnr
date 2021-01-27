pipeline {
  agent any {
    stages {
      stage('Build') {
        steps {
          echo $WORKSPACE
          sudo podman run --rm \
            -v $WORKSPACE:/rpkg \
            fhix/dr:latest /bin/bash -c \
            'cd /rpkg; make build'
        }
      }
      stage('Check') {
        steps {
          sudo podman run --rm \
            -v $WORKSPACE:/rpkg \
            fhix/dr:latest /bin/bash -c \
            'cd /rpkg; make check'
        }
      }
      stage('Clean') {
        steps {
          sudo podman run --rm \
            -v $WORKSPACE:/rpkg \
            fhix/dr:latest /bin/bash -c \
            'cd /rpkg; make clean'
        }
      }
    }
  }
}
