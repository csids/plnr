pipeline {
  agent any
  stages {
    stage('Build') {
      steps {
        sh """
          sudo podman run --rm \
            -v $WORKSPACE:/rpkg \
            fhix/dr:latest /bin/bash -c \
            'cd /rpkg; make build'
        """
      }
    }
    stage('Check') {
      steps {
        sh """
          sudo podman run --rm \
            -v $WORKSPACE:/rpkg \
            fhix/dr:latest /bin/bash -c \
            'cd /rpkg; make check'
        """
      }
    }
    stage('Clean') {
      steps {
        sh """
          sudo podman run --rm \
            -v $WORKSPACE:/rpkg \
            fhix/dr:latest /bin/bash -c \
            'cd /rpkg; make clean'
        """
      }
    }
  }
}
