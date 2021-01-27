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
    stage('drat insert') {
      steps {
        sh """
          sudo podman run --rm \
            -v $WORKSPACE:/rpkg \
            -v /mnt/n/sykdomspulsen_config/drat:/drat \
            fhix/dr:latest /bin/bash -c \
            'cd /rpkg; make drat_insert'
        """
      }
    }
    stage('drat push') {
      steps {
        sh """
          make drat_push
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
